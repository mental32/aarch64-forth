#!/usr/bin/env perl
use strict;
use warnings;
use Scalar::Util qw(looks_like_number);

# Host-side Forth VM that executes the source to build a target dictionary
# image. The VM maintains a symbolic memory (cells/bytes with labels) so we
# can emit relocatable GAS assembly without hand-coded control-flow or
# dictionary construction in Perl.

my $src = shift(@ARGV) // 'program.fs';
open my $fh, '<', $src or die "cannot open $src: $!";

#
# Tokenizer
#
sub tokenize {
    my ($handle) = @_;
    my @toks;
    while (my $line = <$handle>) {
        chomp $line;
        my @chars = split //, $line;
        my $buf = '';
        my $i   = 0;
        while ($i < @chars) {
            my $c = $chars[$i];
            if ($c =~ /\s/) {
                push @toks, $buf if length $buf;
                $buf = '';
                $i++;
                next;
            }
            if ($c eq '\\' && ($i == 0 || $chars[$i-1] =~ /\s/)) {
                push @toks, $buf if length $buf;
                $buf = '';
                last;
            }
            if ($c eq '.' && $i + 1 < @chars && $chars[$i+1] eq '"') {
                push @toks, $buf if length $buf;
                $buf = '';
                $i  += 2;
                # Skip whitespace after ."
                while ($i < @chars && $chars[$i] =~ /\s/) {
                    $i++;
                }
                my $str = '';
                while ($i < @chars && $chars[$i] ne '"') {
                    $str .= $chars[$i];
                    $i++;
                }
                die "missing closing '\"' for string literal" if $i >= @chars;
                push @toks, { type => 'string', text => $str };
                $i++;    # skip closing quote
                next;
            }
            $buf .= $c;
            $i++;
        }
        push @toks, $buf if length $buf;
    }
    return @toks;
}

my @tokens = tokenize($fh);

#
# Symbolic memory model
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
    # Ensure value is properly formatted for assembly emission
    my $cell_value;
    if (ref($v) eq 'HASH') {
        # Keep hash references as-is for later resolution
        $cell_value = $v;
    } elsif (looks_like_number($v)) {
        # Convert numbers to proper format
        $cell_value = mk_num($v);
    } else {
        # Treat as external symbol
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
# Dictionary entries
#
my %dict;          # lowercased name -> word
my %xt_to_word;    # xt offset -> word object
my %hdr_to_word;   # header offset -> word object
my %sym_used;      # assembler symbol usage guard
my $latest_hdr;

#
# Local label support for words
#
my %word_labels;     # word_name -> { label_name => 1 } for defined labels
my %word_label_refs; # word_name -> { label_name => 1 } for referenced labels
my %word_gensym;     # word_name -> counter for unique label generation

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
            die "cannot treat external addr as number" if $k eq 'external';
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

sub add_word {
    my ($word) = @_;
    my $lname = lc $word->{name};
    $dict{$lname}      = $word;
    $xt_to_word{ $word->{xt} } = $word if defined $word->{xt};
    $hdr_to_word{ $word->{header_off} } = $word if defined $word->{header_off};
}

sub add_primitive {
    my ($name, $code_label) = @_;
    my $hdr = "hdr_prim_$code_label";
    my $sym = "xt_$code_label";

    my $hdr_off = emit_cell(defined $latest_hdr ? mk_addr_internal($latest_hdr) : mk_num(0));    # link
    add_label($hdr, $hdr_off);
    emit_byte(0);                                 # flags
    emit_byte(length $name);                      # name len
    emit_byte($_) for map { ord } split //, $name;
    align_mem(8);
    my $cfa_off = emit_cell($code_label);
    add_label($sym, $cfa_off);

    $latest_hdr = $hdr_off;
    register_internal($sym, $cfa_off);

    my $word = {
        name       => $name,
        sym        => $sym,
        xt         => $cfa_off,
        header_off => $hdr_off,
        immediate  => 0,
        kind       => 'prim',
        host       => undef,    # filled later if host semantics available
    };
    add_word($word);
}

#
# Primitives list (target code labels match kernel.S)
#
my @prim_defs = (
    { name => 'lit',            code => 'forth_lit' },
    { name => 'exit',           code => 'forth_exit' },
    { name => 'execute',        code => 'forth_execute' },
    { name => 'emit',           code => 'forth_emit' },
    { name => 'key',            code => 'forth_key' },
    { name => 'next-token',     code => 'forth_next_token' },
    { name => 'find-name',      code => 'forth_find_name' },
    { name => 'parse-num',      code => 'forth_parse_num' },
    { name => 'type',           code => 'forth_type' },

    { name => 'bye',            code => 'forth_bye' },
    { name => '!',              code => 'forth_store' },
    { name => 'dup',            code => 'forth_dup' },
    { name => 'drop',           code => 'forth_drop' },
    { name => 'swap',           code => 'forth_swap' },

    { name => '>r',             code => 'forth_to_r' },
    { name => 'r>',             code => 'forth_r_from' },
    { name => 'r@',             code => 'forth_r_fetch' },
    { name => '+',              code => 'forth_plus' },
    { name => '-',              code => 'forth_minus' },
    { name => 'or',             code => 'forth_or_op' },
    { name => 'and',            code => 'forth_and_op' },
    { name => 'xor',            code => 'forth_xor_op' },
    { name => 'invert',         code => 'forth_invert' },
    { name => '2/',             code => 'forth_two_slash' },

    { name => '@',              code => 'forth_fetch' },
    { name => 'c@',             code => 'forth_c_fetch' },
    { name => 'c!',             code => 'forth_c_store' },
    { name => '0=',             code => 'forth_zero_equal' },
    { name => '0<',             code => 'forth_zero_less' },

    { name => 'branch',         code => 'forth_branch' },
    { name => '0branch',        code => 'forth_zero_branch' },
    { name => 'move',           code => 'forth_move' },
    { name => 'fill',           code => 'forth_fill' },
    { name => 'abs',            code => 'forth_abs' },
    { name => 'min',            code => 'forth_min' },
    { name => 'max',            code => 'forth_max' },
    { name => 'uart-hex',       code => 'forth_uart_hex' },
    { name => 'uart-fr',        code => 'forth_uart_fr' },
    { name => 'uart-dr',        code => 'forth_uart_dr' },
    { name => 'uart-poll',      code => 'forth_uart_poll' },
    { name => 'uart-base',      code => 'forth_uart_base' },
    { name => 'uart-base-reg',  code => 'forth_uart_base_reg' },
    { name => 'uart-ibrd',      code => 'forth_uart_ibrd' },
    { name => 'uart-fbrd',      code => 'forth_uart_fbrd' },
    { name => 'uart-lcrh',      code => 'forth_uart_lcrh' },
    { name => 'uart-cr',        code => 'forth_uart_cr' },
    { name => 'hex64',          code => 'forth_hex64' },
    { name => '.',              code => 'forth_dot' },
    { name => '/',              code => 'forth_div' },
    { name => 'mod',            code => 'forth_mod' },
);

add_primitive($_->{name}, $_->{code}) for @prim_defs;

#
# Variables for runtime (addresses reachable via &name)
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
my $state_addr     = add_cell_var('state', 0);
my $latest_var     = add_cell_var('latest', 0);

# External addresses reachable via &name
register_external($_) for qw(bss_end input_buf input_len input_pos forth_docol);

# dict_here starts at current here
store_cell($dict_here_addr, mk_addr_internal($here));
# latest tracks head of primitive list
my $initial_latest = defined $latest_hdr ? mk_addr_internal($latest_hdr) : mk_num(0);
store_cell($latest_var, $initial_latest);
register_internal('xt_exit', $addr{'xt_forth_exit'}->{off});
register_internal('xt_lit',  $addr{'xt_forth_lit'}->{off});
register_internal('xt_zero_branch', $addr{'xt_forth_zero_branch'}->{off});
register_internal('xt_branch', $addr{'xt_forth_branch'}->{off});
register_internal('xt_forth_exit', $addr{'xt_forth_exit'}->{off});
register_internal('xt_forth_lit', $addr{'xt_forth_lit'}->{off});
register_internal('xt_forth_zero_branch', $addr{'xt_forth_zero_branch'}->{off});
register_internal('xt_forth_branch', $addr{'xt_forth_branch'}->{off});

# Convenience for host primitives that care about dict_here updates
sub current_here { return fetch_cell($dict_here_addr); }

#
# Host stacks
#
my @ds;    # data stack
my @rs;    # return stack

sub push_ds { push @ds, shift; }
sub pop_ds  { pop @ds // die "data stack underflow"; }

#
# Compilation state (needs to be before host_prim for immediate word support)
#
my $compile_state = 0;
my $current_word;

#
# Host primitive implementations (only ones needed at compile time)
#
my %host_prim = (
    'dup'      => sub { my $v = $ds[-1] // die "dup underflow"; push @ds, $v; },
    'drop'     => sub { pop_ds(); },
    'swap'     => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $a, $b; },
    'over'     => sub { my $len = @ds; die "over underflow" if $len < 2; push @ds, $ds[$len-2]; },
    'nip'      => sub { my $a = pop_ds(); pop_ds(); push @ds, $a; },
    'tuck'     => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $a, $b, $a; },
    'rot'      => sub { my $a = pop_ds(); my $b = pop_ds(); my $c = pop_ds(); push @ds, $b, $a, $c; },
    '-rot'     => sub { my $a = pop_ds(); my $b = pop_ds(); my $c = pop_ds(); push @ds, $a, $c, $b; },
    '2dup'     => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $b, $a, $b, $a; },
    '2drop'    => sub { pop_ds(); pop_ds(); },
    '+'        => sub {
        my $a = pop_ds();
        my $b = pop_ds();
        if (is_addr_ref($a) && !is_addr_ref($b)) {
            my $off = addr_off($a) + num_val($b);
            my $k   = addr_kind($a);
            my $res = $k eq 'external' ? mk_addr_external(addr_sym($a), $off)
                                       : mk_addr_internal($off);
            push @ds, $res;
            return;
        }
        if (is_addr_ref($b) && !is_addr_ref($a)) {
            my $off = addr_off($b) + num_val($a);
            my $k   = addr_kind($b);
        my $res = $k eq 'external' ? mk_addr_external(addr_sym($b), $off)
                                       : mk_addr_internal($off);
        push @ds, $res;
        return;
    }
        push @ds, mk_num(num_val($b) + num_val($a));
    },
    '-'        => sub {
        my $a = pop_ds();
        my $b = pop_ds();
        if (is_addr_ref($b) && !is_addr_ref($a)) {
            my $off = addr_off($b) - num_val($a);
            my $k   = addr_kind($b);
            my $res = $k eq 'external' ? mk_addr_external(addr_sym($b), $off)
                                       : mk_addr_internal($off);
            push @ds, $res;
            return;
        }
        push @ds, mk_num(num_val($b) - num_val($a));
    },
    'negate'   => sub { my $a = pop_ds(); push @ds, mk_num(-num_val($a)); },
    '1+'       => sub { my $a = pop_ds(); push @ds, mk_num(num_val($a) + 1); },
    '1-'       => sub { my $a = pop_ds(); push @ds, mk_num(num_val($a) - 1); },
    '2*'       => sub { my $a = pop_ds(); push @ds, mk_num(num_val($a) << 1); },
    '2/'       => sub { my $a = pop_ds(); push @ds, mk_num(num_val($a) >> 1); },
    'invert'   => sub { my $a = pop_ds(); push @ds, mk_num(~num_val($a)); },
    'and'      => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, mk_num(num_val($a) & num_val($b)); },
    'or'       => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, mk_num(num_val($a) | num_val($b)); },
    'xor'      => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, mk_num(num_val($a) ^ num_val($b)); },
    '0='       => sub { my $a = pop_ds(); push @ds, mk_num((num_val($a) == 0) ? -1 : 0); },
    '0<'       => sub { my $a = pop_ds(); push @ds, mk_num((num_val($a) < 0) ? -1 : 0); },
    '='        => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, mk_num((num_val($b) == num_val($a)) ? -1 : 0); },
    '<'        => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, mk_num((num_val($b) < num_val($a)) ? -1 : 0); },
    '>'        => sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, mk_num((num_val($b) > num_val($a)) ? -1 : 0); },
    '@'        => sub {
        my $addr = pop_ds();
        if (is_addr_ref($addr) && $dict_here_addr && addr_kind($addr) eq 'internal' && addr_off($addr) == $dict_here_addr) {
            # Return current here as an address reference for control flow words
            push @ds, mk_addr_internal($here);
            return;
        }
        if (is_addr_ref($addr) && addr_kind($addr) eq 'external') {
            die "cannot fetch external address at compile time";
        }
        my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
        my $val = fetch_cell($resolved);
        push @ds, $val;
    },
    '!'        => sub {
        my $addr = pop_ds();
        my $val  = pop_ds();
        if (is_addr_ref($addr) && addr_kind($addr) eq 'external') {
            die "cannot store to external address at compile time";
        }
        my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
        store_cell($resolved, $val);
        if (defined $dict_here_addr && $resolved == $dict_here_addr) {
            $here = num_val($val);
            _extend_to($here);
        }
    },
    'c@'       => sub {
        my $addr = pop_ds();
        if (is_addr_ref($addr) && addr_kind($addr) eq 'external') {
            die "cannot fetch external byte at compile time";
        }
        my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
        push @ds, fetch_byte($resolved);
    },
    'c!'       => sub {
        my $addr = pop_ds();
        my $val = pop_ds();
        if (is_addr_ref($addr) && addr_kind($addr) eq 'external') {
            die "cannot store to external byte at compile time";
        }
        my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
        store_byte($resolved, num_val($val));
    },
    '+!'       => sub {
        my $addr = pop_ds();
        my $val  = pop_ds();
        my $resolved = is_addr_ref($addr) ? addr_off($addr) : num_val($addr);
        my $cur  = fetch_cell($resolved);
        my $new  = num_val($cur) + num_val($val);
        store_cell($resolved, mk_num($new));
        if (defined $dict_here_addr && $resolved == $dict_here_addr) {
            $here = $new;
            _extend_to($here);
        }
    },
    # Host comma primitive for immediate word execution
    ','        => sub {
        my $val = pop_ds();
        if ($compile_state && $current_word) {
            # During compilation, emit directly and add to body
            push @{ $current_word->{body} }, $val;
            emit_cell($val);  # emit_cell now handles all types properly
        } else {
            # Runtime comma - store at dict_here and update it
            my $dict_here_val = fetch_cell($dict_here_addr);
            my $addr = num_val($dict_here_val);
            store_cell($addr, $val);
            store_cell($dict_here_addr, mk_num($addr + 8));
        }
    },
);

# Attach host semantics to matching primitives
for my $w (values %dict) {
    my $h = $host_prim{ $w->{name} };
    $w->{host} = $h if $h;
}

# Ensure new words can get host semantics after definition
sub attach_host_semantics {
    my ($word) = @_;
    my $h = $host_prim{ $word->{name} };
    $word->{host} = $h if $h;
}

#
# Words created by colon definitions
#
sub current_xt {
    my ($name) = @_;
    my $w = $dict{lc $name} or die "unknown word '$name'";
    return $w->{xt};
}

sub add_colon_word {
    my ($name) = @_;
    my $sym = sanitize_sym($name);

    my $hdr_off = emit_cell(defined $latest_hdr ? mk_addr_internal($latest_hdr) : mk_num(0));    # link
    add_label("hdr_$sym", $hdr_off);
    emit_byte(0);                                 # flags
    emit_byte(length $name);
    emit_byte($_) for map { ord } split //, $name;
    align_mem(8);
    my $cfa_off = emit_cell('forth_docol');
    add_label($sym, $cfa_off);

    $latest_hdr = $hdr_off;
    store_cell($latest_var, mk_addr_internal($hdr_off));
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
    add_word($word);
    attach_host_semantics($word);  # Check for host semantics
    $current_word   = $word;
    $compile_state  = 1;
    # Initialize local label tracking for this word
    $word_labels{$name} = {};
    $word_label_refs{$name} = {};
    $word_gensym{$name} = 0;
    # dict_here points just after cfa
    store_cell($dict_here_addr, mk_addr_internal($here));
}

sub finish_colon_word {
    die "no current word to end" unless $current_word;

    # ensure trailing EXIT
    my $exit_xt = $addr{'xt_forth_exit'}->{off} // die "missing xt_exit address";
    my $body = $current_word->{body};
    if (!@$body || $body->[-1] ne $exit_xt) {
        push @$body, $exit_xt;
        emit_cell(mk_addr_internal($exit_xt));
    }
    $current_word  = undef;
    $compile_state = 0;
}

#
# Compilation helpers
#
sub compile_xt {
    my ($word) = @_;
    $here = num_val(fetch_cell($dict_here_addr)) if defined $dict_here_addr;
    my $xt = $word->{xt};
    push @{ $current_word->{body} }, $xt;
    emit_cell(mk_addr_internal($xt));
}

sub compile_lit {
    my ($val) = @_;
    $here = num_val(fetch_cell($dict_here_addr)) if defined $dict_here_addr;
    my $lit_xt = $addr{'xt_forth_lit'}->{off} // die "missing xt_lit";
    push @{ $current_word->{body} }, $lit_xt, $val;
    emit_cell(mk_addr_internal($lit_xt));
    emit_cell($val);  # emit_cell now handles all types properly
}

my $str_counter = 0;
my @strings;
sub compile_string {
    my ($text) = @_;
    my $label = sprintf("str%d", $str_counter++);
    my $len = length($text);

    # Defer string data emission
    push @strings, {
        label => $label,
        text => $text
    };

    # Compile runtime code: lit address, lit length, type
    compile_lit($label);  # Use label name directly for assembler resolution
    compile_lit(mk_num($len));
    my $type_word = $dict{'type'} or die "missing type word";
    compile_xt($type_word);
}

#
# Local label support functions
#
sub compile_label_def {
    my ($lname) = @_;
    die "no current word for label definition" unless $current_word;
    my $word_name = $current_word->{name};
    my $word_sym = $current_word->{sym};
    $word_labels{$word_name}{$lname} = 1;
    # Create the label at current position
    my $label_name = "${word_sym}_${lname}";
    add_label($label_name, $here);
}

sub compile_label_ref {
    my ($lname) = @_;
    die "no current word for label reference" unless $current_word;
    my $word_name = $current_word->{name};
    my $word_sym = $current_word->{sym};
    $word_label_refs{$word_name}{$lname} = 1;
    # Emit reference to the label
    my $label_name = "${word_sym}_${lname}";
    emit_cell(mk_addr_external($label_name));
}

#
# Execution of colon words (for immediate behavior)
#
sub exec_word {
    my ($word) = @_;
    if ($word->{kind} eq 'prim') {
        die "primitive '$word->{name}' lacks host semantics during execution"
          unless $word->{host};
        $word->{host}->();
        return;
    }
    die "cannot execute colon word without body" unless $word->{body};
    my $body = $word->{body};
    my $ip   = 0;
    while ($ip < @$body) {
        my $cell = $body->[$ip++];

        # Skip label markers and targets during execution
        if (ref($cell) eq 'HASH' && exists $cell->{type}) {
            if ($cell->{type} eq 'label') {
                # Skip label definitions
                next;
            } elsif ($cell->{type} eq 'target') {
                # Skip label references - they don't execute
                next;
            }
        }

        if ($cell eq ($addr{'xt_forth_lit'}->{off} // '')) {
            my $lit = $body->[$ip++] // die "missing literal after xt_lit";
            push_ds($lit);
            next;
        }
        if ($cell eq ($addr{'xt_forth_exit'}->{off} // '')) {
            last;
        }
        my $target = $xt_to_word{$cell}
          or die "unknown xt $cell in word '$word->{name}'";

        # Use host semantics if available during immediate execution
        if ($compile_state && $target->{host}) {
            $target->{host}->();
        } else {
            exec_word($target);
        }
    }
}

sub mark_latest_immediate {
    my $hdr_val = fetch_cell($latest_var);
    my $hdr_off = is_addr_ref($hdr_val) ? addr_off($hdr_val) : num_val($hdr_val);
    my $w   = $hdr_to_word{$hdr_off} or die "no word found for header $hdr_off";
    $w->{immediate} = 1;
    my $flags = fetch_byte($hdr_off + 8);
    store_byte($hdr_off + 8, $flags | 1);
}

#
# Main interpreter loop
#
sub is_immediate_word {
    my ($word) = @_;
    return 1 if $word->{immediate};
    if (defined $word->{header_off}) {
        my $flags = fetch_byte($word->{header_off} + 8);
        return ($flags & 1) ? 1 : 0;
    }
    return 0;
}

sub handle_word_token {
    my ($tok) = @_;
    my $word = $dict{lc $tok}
      or die "unknown word '$tok'";

    if ($compile_state) {
        if (is_immediate_word($word)) {
            exec_word($word);
        } else {
            compile_xt($word);
        }
        return;
    }

    exec_word($word);
    mark_latest_immediate() if lc($tok) eq 'immediate';
}

sub handle_number_token {
    my ($tok) = @_;
    if ($compile_state) {
        compile_lit(0 + $tok);
    } else {
        push_ds(0 + $tok);
    }
}

sub lookup_addr {
    my ($tok) = @_;
    my ($name) = $tok =~ /^&(.+)$/;
    my $info = $addr{$name} or die "unknown &label '$name'";
    return $info->{type} eq 'internal'
      ? mk_addr_internal($info->{off})
      : mk_addr_external($info->{sym}, 0);
}

TOKEN:
while (@tokens) {
    my $tok = shift @tokens;

    # Inline comment: ( ... ) skip until token with ')'
    if (defined $tok && $tok eq '(') {
        while (@tokens) {
            my $t = shift @tokens;
            last if defined($t) && $t =~ /\)/;
        }
        next TOKEN;
    }

    if (ref($tok) eq 'HASH' && $tok->{type} && $tok->{type} eq 'string') {
        if ($compile_state) {
            compile_string($tok->{text});
        } else {
            # Interpretation mode: same as compile mode now that compile_string is fixed
            compile_string($tok->{text});
        }
        next TOKEN;
    }

    if ($tok eq ':') {
        my $name = shift(@tokens) // die "':' without name";
        add_colon_word($name);
        next TOKEN;
    }
    if ($tok eq ';') {
        finish_colon_word();
        next TOKEN;
    }

    if ($tok =~ /^&[A-Za-z_]\w*$/) {
        my $addr_val = lookup_addr($tok);
        if ($compile_state) {
            compile_lit($addr_val);
        } else {
            push_ds($addr_val);
        }
        next TOKEN;
    }

    # Local label definition: label:name
    if ($tok =~ /^label:([A-Za-z_]\w*)$/) {
        die "label definition outside colon definition" unless $compile_state;
        compile_label_def($1);
        next TOKEN;
    }

    # Local label reference: >name (but not >r which is a primitive)
    if ($tok =~ /^>([A-Za-z_]\w+)$/ && $tok ne '>r') {
        die "label reference outside colon definition" unless $compile_state;
        compile_label_ref($1);
        next TOKEN;
    }



    if (looks_like_number($tok) && $tok =~ /^-?\d+$/) {
        handle_number_token($tok);
        next TOKEN;
    }

    handle_word_token($tok);
}

die "unterminated definition at EOF" if $compile_state;

#
# Final touches: latest variable and entry_thread
#
my $latest_addr = defined $latest_hdr ? mk_addr_internal($latest_hdr) : mk_num(0);
store_cell($latest_var, $latest_addr);

align_mem(8);
add_label('entry_thread', $here);
my $boot_word = $dict{'boot'} or die "no word named 'boot' defined";
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

# All primitives are now implemented in kernel.S
# We only need to declare external references here
print <<'EXTERNS';
.extern forth_lit
.extern forth_exit
.extern forth_docol
.extern forth_execute
.extern forth_emit
.extern forth_key
.extern forth_next_token
.extern forth_find_name
.extern forth_parse_num
.extern forth_type

.extern forth_bye
.extern forth_store
.extern forth_dup
.extern forth_drop
.extern forth_swap
.extern forth_to_r
.extern forth_r_from
.extern forth_r_fetch
.extern forth_plus
.extern forth_minus
.extern forth_or_op
.extern forth_and_op
.extern forth_xor_op
.extern forth_invert
.extern forth_two_slash
.extern forth_fetch
.extern forth_c_fetch
.extern forth_c_store
.extern forth_zero_equal
.extern forth_zero_less
.extern forth_branch
.extern forth_zero_branch
.extern forth_move
.extern forth_fill
.extern forth_abs
.extern forth_min
.extern forth_max
.extern forth_uart_hex
.extern forth_uart_fr
.extern forth_uart_dr
.extern forth_uart_poll
.extern forth_uart_base
.extern forth_uart_base_reg
.extern forth_uart_ibrd
.extern forth_uart_fbrd
.extern forth_uart_lcrh
.extern forth_uart_cr
.extern forth_hex64
.extern forth_dot
.extern forth_div
.extern forth_mod
EXTERNS

#
# Emit data section from symbolic memory
#
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

# Emit deferred strings
for my $s (@strings) {
    my $label = $s->{label};
    my $text = $s->{text};
    print ".global $label\n";
    print "$label:\n";
    for my $char (split //, $text) {
        printf "\t.byte %d\n", ord($char);
    }
    # Align to 8-byte boundary
    my $len = length($text);
    my $padding = (8 - ($len % 8)) % 8;
    for (1..$padding) {
        print "\t.byte 0\n";
    }
}

print "\n";
