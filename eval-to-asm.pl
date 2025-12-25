#!/usr/bin/env perl
use strict;
use warnings;
use Scalar::Util qw(looks_like_number);
use File::Basename qw(dirname);
use File::Spec;
use FindBin;
use lib $FindBin::Bin;
use Eval;

# Host-side compile driver that mirrors the Eval.pm VM into a symbolic
# target memory image and emits GAS assembly.

my $src = shift(@ARGV) // 'kernel.fth';

sub parse_kernel_prims {
    my ($path) = @_;
    open my $fh, '<', $path or die "cannot open $path: $!";
    my %seen;
    my @labels;
    my %prim_name;
    my %prim_skip;
    while (my $line = <$fh>) {
        if ($line =~ m{//\s*prim:\s*(forth_[A-Za-z0-9_]+)\s+(\S+)}) {
            my ($label, $name) = ($1, $2);
            die "duplicate prim mapping for $label" if exists $prim_name{$label};
            $prim_name{$label} = $name;
        }
        if ($line =~ m{//\s*prim-skip:\s*(forth_[A-Za-z0-9_]+)}) {
            $prim_skip{$1} = 1;
        }
        if ($line =~ /^\s*\.global\s+(forth_[A-Za-z0-9_]+)/) {
            my $label = $1;
            next if $seen{$label}++;
            push @labels, $label;
        }
    }
    close $fh;
    die "no forth_* globals found in $path" unless @labels;
    for my $label (keys %prim_name) {
        die "prim mapping for unknown label $label" unless $seen{$label};
    }
    return (\@labels, \%prim_name, \%prim_skip);
}

my $script_dir = dirname(File::Spec->rel2abs($0));
my $kernel_path = File::Spec->catfile($script_dir, 'kernel.S');
my ($kernel_forth_labels_ref, $kernel_prim_name_ref, $kernel_prim_skip_ref) =
  parse_kernel_prims($kernel_path);
my @kernel_forth_labels = @$kernel_forth_labels_ref;
my %kernel_prim_name = %$kernel_prim_name_ref;
my %kernel_prim_skip = %$kernel_prim_skip_ref;
my %kernel_label_seen = map { $_ => 1 } @kernel_forth_labels;
die "kernel missing forth_docol in $kernel_path" unless $kernel_label_seen{forth_docol};
die "kernel missing prim-skip for forth_docol in $kernel_path"
  unless $kernel_prim_skip{forth_docol};

#
# Symbolic memory model for the target image
#
my @mem;            # list of { offset, size, kind => 'cell'|'byte', value }
my %labels;         # offset -> label name
my $here = 0;       # next free offset (bytes)
my $dict_here_addr; # set after variables are allocated

sub add_label {
    my ($name, $off) = @_;
    die "duplicate label $name" if grep { $_ eq $name } values %labels;
    die "offset $off already has label $labels{$off}" if exists $labels{$off};
    $labels{$off} = $name;
}

sub sync_dict_here {
    return unless defined $dict_here_addr;
    store_cell($dict_here_addr, mk_addr_internal($here));
}

sub align_mem {
    my ($n) = @_;
    while ($here % $n) {
        push @mem, { offset => $here, size => 1, kind => 'byte', value => 0 };
        $here++;
        sync_dict_here();
    }
}

sub emit_byte {
    my ($v) = @_;
    push @mem, { offset => $here, size => 1, kind => 'byte', value => $v & 0xFF };
    $here++;
    sync_dict_here();
    return $here - 1;
}

sub emit_cell {
    my ($v) = @_;
    align_mem(8);
    my $off = $here;
    my $cell_value;
    if (ref($v) eq 'HASH') {
        $cell_value = $v;
    } elsif (looks_like_number($v)) {
        $cell_value = mk_num($v);
    } else {
        $cell_value = mk_addr_external($v);
    }
    push @mem, { offset => $off, size => 8, kind => 'cell', value => $cell_value };
    $here += 8;
    sync_dict_here();
    return $off;
}

sub _extend_to {
    my ($target) = @_;
    while ($here < $target) {
        emit_byte(0);
    }
}

sub store_cell {
    my ($addr, $val) = @_;
    die "store_cell misaligned addr=$addr" if $addr % 8 != 0;
    my $end = $addr + 8;
    _extend_to($end) if $end > $here;
    @mem = grep {
        my $s = $_->{offset};
        my $e = $s + $_->{size};
        !($s >= $addr && $s < $end);
    } @mem;
    push @mem, { offset => $addr, size => 8, kind => 'cell', value => $val };
    @mem = sort { $a->{offset} <=> $b->{offset} } @mem;
}

sub store_byte {
    my ($addr, $val) = @_;
    _extend_to($addr + 1) if $addr + 1 > $here;
    @mem = grep { $_->{offset} != $addr } @mem;
    push @mem, { offset => $addr, size => 1, kind => 'byte', value => $val & 0xFF };
    @mem = sort { $a->{offset} <=> $b->{offset} } @mem;
}

sub fetch_cell {
    my ($addr) = @_;
    for my $u (@mem) {
        next unless $u->{offset} == $addr;
        if ($u->{kind} eq 'cell') {
            my $v = $u->{value};
            return $v;
        }
        die "fetch_cell at $addr hits byte data";
    }
    die "fetch_cell unknown addr $addr";
}

sub fetch_byte {
    my ($addr) = @_;
    for my $u (@mem) {
        next unless $u->{offset} == $addr;
        return $u->{value} if $u->{kind} eq 'byte';
        die "fetch_byte at $addr hits cell";
    }
    die "fetch_byte unknown addr $addr";
}

#
# Address table for &name tokens
#
my %addr;    # name -> { type => 'internal', off => ... } | { type => 'external', sym => ... }

sub register_internal {
    my ($name, $off) = @_;
    $addr{$name} = { type => 'internal', off => $off };
}

sub register_external {
    my ($name) = @_;
    $addr{$name} = { type => 'external', sym => $name };
}

#
# Value helpers
#
my %sym_used;

sub mk_num  { return { num  => 0 + $_[0] }; }
sub mk_addr_internal { return { addr => { kind => 'internal', off => $_[0] } }; }
sub mk_addr_external { my ($sym, $off) = @_; $off //= 0; return { addr => { kind => 'external', sym => $sym, off => $off } }; }

sub is_addr_ref {
    my ($v) = @_;
    return ref($v) eq 'HASH' && exists $v->{addr};
}

sub addr_off {
    my ($v) = @_;
    die "addr_off on non-addr" unless is_addr_ref($v);
    my $a = $v->{addr};
    return $a->{off} // 0;
}

sub addr_kind {
    my ($v) = @_;
    die "addr_kind on non-addr" unless is_addr_ref($v);
    return $v->{addr}->{kind};
}

sub addr_sym {
    my ($v) = @_;
    die "addr_sym on non-addr" unless is_addr_ref($v);
    return $v->{addr}->{sym};
}

sub num_val {
    my ($v) = @_;
    if (ref($v) eq 'HASH') {
        return $v->{num} if exists $v->{num};
        if (is_addr_ref($v)) {
            my $k = addr_kind($v);
            die "cannot treat external addr as number ($v->{addr}->{sym})" if $k eq 'external';
            return addr_off($v);
        }
        die "num_val unknown hash";
    }
    return 0 + $v if looks_like_number($v);
    die "num_val on non-numeric scalar";
}

sub sanitize_sym {
    my ($name) = @_;
    (my $base = $name) =~ s/[^A-Za-z0-9_]/_/g;
    my $sym = "w_$base";
    my $n   = 0;
    while ($sym_used{$sym}) {
        $n++;
        $sym = "w_${base}_$n";
    }
    $sym_used{$sym} = 1;
    return $sym;
}

#
# Target dictionary entries
#
my %t_dict;          # lowercased name -> word
my $t_latest_hdr;
my $t_current_word;
my $t_compile_state = 0;

my %word_labels;
my %word_label_refs;
my %word_gensym;

sub t_add_word {
    my ($word) = @_;
    $t_dict{lc $word->{name}} = $word;
}

sub t_add_primitive {
    my ($name, $code_label) = @_;
    my $hdr = "hdr_prim_$code_label";
    my $sym = "xt_$code_label";

    my $hdr_off = emit_cell(defined $t_latest_hdr ? mk_addr_internal($t_latest_hdr) : mk_num(0));
    add_label($hdr, $hdr_off);
    emit_byte(0);
    emit_byte(length $name);
    emit_byte($_) for map { ord } split //, $name;
    align_mem(8);
    my $cfa_off = emit_cell($code_label);
    add_label($sym, $cfa_off);

    $t_latest_hdr = $hdr_off;
    register_internal($sym, $cfa_off);

    my $word = {
        name       => $name,
        sym        => $sym,
        xt         => $cfa_off,
        header_off => $hdr_off,
        immediate  => 0,
        kind       => 'prim',
        body       => [],
    };
    t_add_word($word);
    return $word;
}

sub t_add_colon_word {
    my ($name) = @_;
    my $sym = sanitize_sym($name);

    my $hdr_off = emit_cell(defined $t_latest_hdr ? mk_addr_internal($t_latest_hdr) : mk_num(0));
    add_label("hdr_$sym", $hdr_off);
    emit_byte(0);
    emit_byte(length $name);
    emit_byte($_) for map { ord } split //, $name;
    align_mem(8);
    my $cfa_off = emit_cell('forth_docol');
    add_label($sym, $cfa_off);

    $t_latest_hdr = $hdr_off;
    register_internal($sym, $cfa_off);

    my $word = {
        name       => $name,
        sym        => $sym,
        xt         => $cfa_off,
        header_off => $hdr_off,
        immediate  => 0,
        kind       => 'colon',
        body       => [],
    };
    t_add_word($word);
    $t_current_word = $word;
    $t_compile_state = 1;
    $word_labels{$name} = {};
    $word_label_refs{$name} = {};
    $word_gensym{$name} = 0;
    store_cell($dict_here_addr, mk_addr_internal($here)) if defined $dict_here_addr;
    return $word;
}

sub t_add_create_word {
    my ($name) = @_;
    my $sym = sanitize_sym($name);

    my $hdr_off = emit_cell(defined $t_latest_hdr ? mk_addr_internal($t_latest_hdr) : mk_num(0));
    add_label("hdr_$sym", $hdr_off);
    emit_byte(0);
    emit_byte(length $name);
    emit_byte($_) for map { ord } split //, $name;
    align_mem(8);
    my $cfa_off = emit_cell('forth_dodoes');
    add_label($sym, $cfa_off);

    emit_cell(mk_num(0));
    my $pfa_off = $here;

    $t_latest_hdr = $hdr_off;
    register_internal($sym, $cfa_off);

    my $word = {
        name       => $name,
        sym        => $sym,
        xt         => $cfa_off,
        header_off => $hdr_off,
        immediate  => 0,
        kind       => 'prim',
        body       => [],
        pfa_off    => $pfa_off,
    };
    t_add_word($word);
    return $word;
}

sub t_finish_colon_word {
    die "no target word to end" unless $t_current_word;
    my $exit_xt = $addr{'xt_forth_exit'}->{off} // die "missing xt_forth_exit";
    my $body = $t_current_word->{body};
    if (!@$body || ref($body->[-1]) || $body->[-1] ne $exit_xt) {
        push @$body, $exit_xt;
        emit_cell(mk_addr_internal($exit_xt));
    }
    $t_current_word  = undef;
    $t_compile_state = 0;
}

sub t_compile_label_def {
    my ($lname) = @_;
    die "no current word for label definition" unless $t_current_word;
    my $word_name = $t_current_word->{name};
    my $word_sym = $t_current_word->{sym};
    $word_labels{$word_name}{$lname} = 1;
    my $label_name = "${word_sym}_${lname}";
    add_label($label_name, $here);
}

sub t_compile_label_ref {
    my ($lname) = @_;
    die "no current word for label reference" unless $t_current_word;
    my $word_name = $t_current_word->{name};
    my $word_sym = $t_current_word->{sym};
    $word_label_refs{$word_name}{$lname} = 1;
    my $label_name = "${word_sym}_${lname}";
    emit_cell(mk_addr_external($label_name));
    push @{ $t_current_word->{body} }, mk_addr_external($label_name);
}

sub target_lookup_addr {
    my ($name) = @_;
    my $info = $addr{$name} or die "unknown &label '$name'";
    return $info->{type} eq 'internal'
      ? mk_addr_internal($info->{off})
      : mk_addr_external($info->{sym}, 0);
}

#
# Seed target dictionary with kernel primitives
#
my @prim_defs;
for my $label (@kernel_forth_labels) {
    next if $kernel_prim_skip{$label};
    my $name = $kernel_prim_name{$label}
      // die "missing prim mapping for $label in $kernel_path";
    push @prim_defs, { name => $name, code => $label };
}

my %t_prim_by_name;
for my $p (@prim_defs) {
    my $word = t_add_primitive($p->{name}, $p->{code});
    $t_prim_by_name{lc $p->{name}} = $word;
}

#
# Target variables (reachable via &name)
#
sub add_cell_var {
    my ($name, $init) = @_;
    align_mem(8);
    my $off = emit_cell($init // 0);
    add_label($name, $off);
    register_internal($name, $off);
    return $off;
}

sub add_byte_var {
    my ($name, $init) = @_;
    my $off = emit_byte($init // 0);
    add_label($name, $off);
    register_internal($name, $off);
    return $off;
}

add_cell_var('var_cell', 0);
add_byte_var('var_byte', 0);
add_cell_var('var_pair', 0);
$dict_here_addr = add_cell_var('dict_here', 0);
my $state_addr = add_cell_var('state', 0);
my $latest_var = add_cell_var('latest', 0);

register_external($_) for qw(bss_end input_buf input_len input_pos forth_docol forth_dodoes);

store_cell($dict_here_addr, mk_addr_internal($here));
my $initial_latest = defined $t_latest_hdr ? mk_addr_internal($t_latest_hdr) : mk_num(0);
store_cell($latest_var, $initial_latest);

# Convenience aliases
if (exists $addr{'xt_forth_exit'}) {
    register_internal('xt_exit', $addr{'xt_forth_exit'}->{off});
}
if (exists $addr{'xt_forth_lit'}) {
    register_internal('xt_lit', $addr{'xt_forth_lit'}->{off});
}

#
# Target string pool
#
my $str_counter = 0;
my @strings;

# Map target does> addresses to host addresses for host execution.
my %does_target_to_host;

#
# Host semantics (compile-time execution)
#
sub ds_peek {
    return $Eval::ds[-1] // die "data stack underflow";
}

sub host_dup {
    my $v = $Eval::ds[-1] // die "dup underflow";
    Eval::push_ds($v);
}

sub host_drop {
    Eval::pop_ds();
}

sub host_swap {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds($a, $b);
}

sub host_over {
    my $len = scalar @Eval::ds;
    die "over underflow" if $len < 2;
    Eval::push_ds($Eval::ds[$len - 2]);
}

sub host_nip {
    my $a = Eval::pop_ds();
    Eval::pop_ds();
    Eval::push_ds($a);
}

sub host_tuck {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds($a, $b, $a);
}

sub host_rot {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    my $c = Eval::pop_ds();
    Eval::push_ds($b, $a, $c);
}

sub host_mrot {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    my $c = Eval::pop_ds();
    Eval::push_ds($a, $c, $b);
}

sub host_2dup {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds($b, $a, $b, $a);
}

sub host_2drop {
    Eval::pop_ds();
    Eval::pop_ds();
}

sub host_add {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    if (is_addr_ref($a) && !is_addr_ref($b)) {
        my $off = addr_off($a) + num_val($b);
        my $k   = addr_kind($a);
        my $res = $k eq 'external' ? mk_addr_external(addr_sym($a), $off)
                                   : mk_addr_internal($off);
        Eval::push_ds($res);
        return;
    }
    if (is_addr_ref($b) && !is_addr_ref($a)) {
        my $off = addr_off($b) + num_val($a);
        my $k   = addr_kind($b);
        my $res = $k eq 'external' ? mk_addr_external(addr_sym($b), $off)
                                   : mk_addr_internal($off);
        Eval::push_ds($res);
        return;
    }
    Eval::push_ds(mk_num(num_val($b) + num_val($a)));
}

sub host_sub {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    if (is_addr_ref($b) && !is_addr_ref($a)) {
        my $off = addr_off($b) - num_val($a);
        my $k   = addr_kind($b);
        my $res = $k eq 'external' ? mk_addr_external(addr_sym($b), $off)
                                   : mk_addr_internal($off);
        Eval::push_ds($res);
        return;
    }
    Eval::push_ds(mk_num(num_val($b) - num_val($a)));
}

sub host_negate {
    my $a = Eval::pop_ds();
    Eval::push_ds(mk_num(-num_val($a)));
}

sub host_1p {
    my $a = Eval::pop_ds();
    Eval::push_ds(mk_num(num_val($a) + 1));
}

sub host_1m {
    my $a = Eval::pop_ds();
    Eval::push_ds(mk_num(num_val($a) - 1));
}

sub host_2m {
    my $a = Eval::pop_ds();
    Eval::push_ds(mk_num(num_val($a) << 1));
}

sub host_2d {
    my $a = Eval::pop_ds();
    Eval::push_ds(mk_num(num_val($a) >> 1));
}

sub host_invert {
    my $a = Eval::pop_ds();
    Eval::push_ds(mk_num(~num_val($a)));
}

sub host_and {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds(mk_num(num_val($a) & num_val($b)));
}

sub host_or {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds(mk_num(num_val($a) | num_val($b)));
}

sub host_xor {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds(mk_num(num_val($a) ^ num_val($b)));
}

sub host_0eq {
    my $a = Eval::pop_ds();
    Eval::push_ds(mk_num((num_val($a) == 0) ? -1 : 0));
}

sub host_0lt {
    my $a = Eval::pop_ds();
    Eval::push_ds(mk_num((num_val($a) < 0) ? -1 : 0));
}

sub host_eq {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds(mk_num((num_val($b) == num_val($a)) ? -1 : 0));
}

sub host_lt {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds(mk_num((num_val($b) < num_val($a)) ? -1 : 0));
}

sub host_gt {
    my $a = Eval::pop_ds();
    my $b = Eval::pop_ds();
    Eval::push_ds(mk_num((num_val($b) > num_val($a)) ? -1 : 0));
}

sub host_fetch {
    my $addr = Eval::pop_ds();
    if (is_addr_ref($addr) && defined $dict_here_addr && addr_kind($addr) eq 'internal' && addr_off($addr) == $dict_here_addr) {
        Eval::push_ds(mk_addr_internal($here));
        return;
    }
    if (is_addr_ref($addr) && addr_kind($addr) eq 'external') {
        die "cannot fetch external address at compile time";
    }
    my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
    my $val = fetch_cell($resolved);
    Eval::push_ds($val);
}

sub host_store {
    my $addr = Eval::pop_ds();
    my $val  = Eval::pop_ds();
    if (is_addr_ref($addr) && addr_kind($addr) eq 'external') {
        die "cannot store to external address at compile time";
    }
    my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
    store_cell($resolved, $val);
    if (defined $dict_here_addr && $resolved == $dict_here_addr) {
        if (is_addr_ref($val) && addr_kind($val) eq 'external') {
            return;
        }
        $here = num_val($val);
        _extend_to($here);
    }
}

sub host_cfetch {
    my $addr = Eval::pop_ds();
    if (is_addr_ref($addr) && addr_kind($addr) eq 'external') {
        die "cannot fetch external byte at compile time";
    }
    my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
    Eval::push_ds(fetch_byte($resolved));
}

sub host_cstore {
    my $addr = Eval::pop_ds();
    my $val = Eval::pop_ds();
    if (is_addr_ref($addr) && addr_kind($addr) eq 'external') {
        die "cannot store to external byte at compile time";
    }
    my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
    store_byte($resolved, num_val($val));
}

sub host_plus_store {
    my $addr = Eval::pop_ds();
    my $val  = Eval::pop_ds();
    my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
    my $cur  = fetch_cell($resolved);
    my $new  = num_val($cur) + num_val($val);
    store_cell($resolved, mk_num($new));
    if (defined $dict_here_addr && $resolved == $dict_here_addr) {
        $here = $new;
        _extend_to($here);
    }
}

sub host_comma {
    my $val = Eval::pop_ds();
    if ($t_compile_state && $t_current_word) {
        push @{ $t_current_word->{body} }, $val;
        emit_cell($val);
    } else {
        my $dict_here_val = fetch_cell($dict_here_addr);
        my $addr = num_val($dict_here_val);
        store_cell($addr, $val);
        store_cell($dict_here_addr, mk_num($addr + 8));
    }
}

sub host_here {
    Eval::push_ds(mk_addr_internal($here));
}

sub read_mem_string {
    my ($addr, $len) = @_;
    my $off;
    if (is_addr_ref($addr)) {
        die "included cannot use external address" if addr_kind($addr) eq 'external';
        $off = addr_off($addr);
    } else {
        $off = num_val($addr);
    }
    my $n = num_val($len);
    my $s = '';
    for (my $i = 0; $i < $n; $i++) {
        $s .= chr(fetch_byte($off + $i));
    }
    return $s;
}

sub host_included {
    my $len = Eval::pop_ds();
    my $addr = Eval::pop_ds();
    my $name = read_mem_string($addr, $len);
    Eval::push_source($name);
}

sub host_create {
    my $tok = Eval::next_token();
    die "create missing name" unless defined $tok;
    die "create name must be a token" if ref($tok);

    my $t_word = t_add_create_word($tok);
    my $t_pfa_off = $t_word->{pfa_off};

    my ($hdr, $cfa) = Eval::create_header($tok, 0);
    my $host_word = {
        name      => $tok,
        kind      => 'prim',
        prim      => sub {
            Eval::push_ds(mk_addr_internal($t_pfa_off));
            my $does_cell = fetch_cell($t_word->{xt} + 8);
            my $does_off = ref($does_cell) ? num_val($does_cell) : $does_cell;
            return if $does_off == 0;
            my $host_addr = $does_target_to_host{$does_off}
              or die "missing host does> address for $does_off";
            Eval::run_thread_at($host_addr);
        },
        immediate => 0,
        xt        => $cfa,
        header    => $hdr,
        pfa       => $cfa + 8,
    };
    Eval::add_word($host_word);
    $host_word->{target_xt} = $t_word->{xt};
    $host_word->{target_header} = $t_word->{header_off};
}

sub host_does {
    die "does> outside colon definition" unless $t_compile_state && $t_current_word;

    my $host_lit_xt_addr = Eval::read_cell($Eval::addr{dict_here});
    my $target_lit_xt_addr = num_val(fetch_cell($dict_here_addr));
    my $lit_body_index = scalar @{ $t_current_word->{body} };

    Eval::compile_lit(0, { kind => 'number', text => '0' });
    my $do_word = Eval::lookup_word('do-does') or die "missing do-does";
    Eval::compile_xt($do_word);

    my $host_does_addr = Eval::read_cell($Eval::addr{dict_here});
    my $target_does_addr = mk_addr_internal(num_val(fetch_cell($dict_here_addr)));

    Eval::write_cell($host_lit_xt_addr + 8, $target_does_addr);
    store_cell($target_lit_xt_addr + 8, $target_does_addr);
    $t_current_word->{body}[$lit_body_index + 1] = $target_does_addr;
    $does_target_to_host{num_val($target_does_addr)} = $host_does_addr;
}

sub host_do_does {
    my $does_addr = Eval::pop_ds();
    my $hdr_val = fetch_cell($latest_var);
    my $hdr_off = is_addr_ref($hdr_val) ? addr_off($hdr_val) : num_val($hdr_val);
    die "do-does without latest word" if $hdr_off == 0;
    my $name_len = fetch_byte($hdr_off + 9);
    my $cfa_off = $hdr_off + 10 + $name_len;
    $cfa_off = ($cfa_off + 7) & ~7;
    store_cell($cfa_off + 8, $does_addr);
    Eval::force_exit();
}

sub host_immediate {
    my $hdr_val = fetch_cell($latest_var);
    my $hdr_off = is_addr_ref($hdr_val) ? addr_off($hdr_val) : num_val($hdr_val);
    die "immediate: no latest word" if $hdr_off == 0;
    my $flags = fetch_byte($hdr_off + 8);
    store_byte($hdr_off + 8, $flags | 1);

    my $host_hdr = Eval::read_cell($Eval::addr{latest});
    my $host_word = Eval::word_for_header($host_hdr);
    if ($host_word) {
        $host_word->{immediate} = 1;
        my $host_flags = Eval::read_byte($host_hdr + 8);
        Eval::write_byte($host_hdr + 8, $host_flags | 1);
    }
}

#
# Hooked compilation into target memory
#
my %hooks = (
    resolve_addr => sub {
        my ($name) = @_;
        return target_lookup_addr($name);
    },
    start_colon => sub {
        my ($host_word) = @_;
        my $t_word = t_add_colon_word($host_word->{name});
        $host_word->{target_xt} = $t_word->{xt};
        $host_word->{target_header} = $t_word->{header_off};
    },
    finish_colon => sub {
        t_finish_colon_word();
    },
    compile_xt => sub {
        my ($host_word) = @_;
        return unless $t_compile_state && $t_current_word;
        my $t_xt = $host_word->{target_xt};
        die "missing target xt for '$host_word->{name}'" unless defined $t_xt;
        push @{ $t_current_word->{body} }, $t_xt;
        emit_cell(mk_addr_internal($t_xt));
    },
    compile_lit => sub {
        my ($val, $meta) = @_;
        return unless $t_compile_state && $t_current_word;
        my $emit_val = $val;
        if ($meta && $meta->{kind} && $meta->{kind} eq 'string_addr') {
            my $label = sprintf("str%d", $str_counter++);
            push @strings, { label => $label, text => $meta->{text} };
            $emit_val = $label;
        } elsif ($meta && $meta->{kind} && $meta->{kind} eq 'string_len') {
            $emit_val = mk_num($val);
        } else {
            $emit_val = mk_num($emit_val) unless ref($emit_val);
        }
        push @{ $t_current_word->{body} }, $emit_val;
        emit_cell($emit_val);
    },
    label_def => sub {
        my ($name) = @_;
        t_compile_label_def($name);
    },
    label_ref => sub {
        my ($name) = @_;
        t_compile_label_ref($name);
    },
);

my %host_semantics = (
    'dup' => \&host_dup,
    'drop' => \&host_drop,
    'swap' => \&host_swap,
    'over' => \&host_over,
    'nip' => \&host_nip,
    'tuck' => \&host_tuck,
    'rot' => \&host_rot,
    '-rot' => \&host_mrot,
    '2dup' => \&host_2dup,
    '2drop' => \&host_2drop,
    '+' => \&host_add,
    '-' => \&host_sub,
    'negate' => \&host_negate,
    '1+' => \&host_1p,
    '1-' => \&host_1m,
    '2*' => \&host_2m,
    '2/' => \&host_2d,
    'invert' => \&host_invert,
    'and' => \&host_and,
    'or' => \&host_or,
    'xor' => \&host_xor,
    '0=' => \&host_0eq,
    '0<' => \&host_0lt,
    '=' => \&host_eq,
    '<' => \&host_lt,
    '>' => \&host_gt,
    '@' => \&host_fetch,
    '!' => \&host_store,
    'c@' => \&host_cfetch,
    'c!' => \&host_cstore,
    '+!' => \&host_plus_store,
    ',' => \&host_comma,
    'here' => \&host_here,
    'included' => \&host_included,
    'create' => \&host_create,
    'does>' => \&host_does,
    'do-does' => \&host_do_does,
    'immediate' => \&host_immediate,
);

Eval::run_file($src, {
    hooks => \%hooks,
    post_init => sub {
        for my $name (keys %host_semantics) {
            Eval::register_host_semantics($name, $host_semantics{$name});
        }

        for my $label (@kernel_forth_labels) {
            next if $kernel_prim_skip{$label};
            my $name = $kernel_prim_name{$label}
              // die "missing prim mapping for $label in $kernel_path";
            if (!Eval::has_word($name)) {
                Eval::add_primitive($name, sub { die "primitive '$name' unavailable on host"; }, 0);
            }
        }

        for my $host_only (qw(does> included)) {
            if (!Eval::has_word($host_only)) {
                Eval::add_primitive($host_only, sub { die "host-only '$host_only' missing semantics"; }, 1);
            }
        }

        for my $label (@kernel_forth_labels) {
            next if $kernel_prim_skip{$label};
            my $name = $kernel_prim_name{$label};
            my $host_word = Eval::lookup_word($name) or die "missing host word $name";
            my $t_word = $t_dict{lc $name} or die "missing target word $name";
            $host_word->{target_xt} = $t_word->{xt};
            $host_word->{target_header} = $t_word->{header_off};
        }
    },
});

#
# Finalize target latest and entry point
#
my $latest_addr = defined $t_latest_hdr ? mk_addr_internal($t_latest_hdr) : mk_num(0);
store_cell($latest_var, $latest_addr);

align_mem(8);
add_label('entry_thread', $here);
my $boot_word = $t_dict{'boot'} or die "no word named 'boot' defined";
emit_cell(mk_addr_internal($boot_word->{xt}));

#
# Assembly emission helpers
#
sub cell_value_asm {
    my ($v) = @_;
    if (!defined $v) {
        return "0";
    }
    if (ref($v) eq 'HASH') {
        if (exists $v->{num}) {
            return $v->{num};
        }
        if (is_addr_ref($v)) {
            my $kind = addr_kind($v);
            my $off  = addr_off($v);
            if ($kind eq 'internal') {
                if (exists $labels{$off}) {
                    return $labels{$off};
                }
                return "dict_base" if $off == 0;
                return "dict_base + $off";
            } else {
                my $sym = addr_sym($v);
                return $sym if $off == 0;
                return "$sym + $off";
            }
        }
        die "unknown cell value hash";
    }
    if (looks_like_number($v)) {
        return $v + 0;
    }
    return $v;
}

for my $sym (@kernel_forth_labels) {
    print ".extern $sym\n";
}

print ".data\n";
print ".align 3\n";
print "dict_base:\n";

@mem = sort { $a->{offset} <=> $b->{offset} } @mem;
my $pos = 0;
for my $u (@mem) {
    if ($u->{offset} > $pos) {
        my $gap = $u->{offset} - $pos;
        for (1 .. $gap) {
            print "\t.byte 0\n";
        }
        $pos = $u->{offset};
    }
    if (my $lab = $labels{$u->{offset}}) {
        print ".global $lab\n";
        print "$lab:\n";
    }
    if ($u->{kind} eq 'cell') {
        my $val_str = cell_value_asm($u->{value});
        print "\t.quad $val_str\n";
    } else {
        print "\t.byte $u->{value}\n";
    }
    $pos += $u->{size};
}

for my $s (@strings) {
    my $label = $s->{label};
    my $text = $s->{text};
    print ".global $label\n";
    print "$label:\n";
    for my $char (split //, $text) {
        printf "\t.byte %d\n", ord($char);
    }
    my $len = length($text);
    my $padding = (8 - ($len % 8)) % 8;
    for (1..$padding) {
        print "\t.byte 0\n";
    }
}

print "\n";
