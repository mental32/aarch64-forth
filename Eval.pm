package Eval;
use strict;
use warnings;
use Scalar::Util qw(looks_like_number);
use File::Basename qw(dirname);
use File::Spec;

our ($src, $halt, $ip, $mem_base, $mem_size, $here, $data_here,
     $latest_header, $compile_state, $current_word, $force_exit, $last_token);
our (@source_stack, @ds, @rs, @mem, @cstack);
our (%addr, %dict, %xt_to_word, %hdr_to_word, %cur_labels, %cur_label_refs);
our (%host_semantics, %cell_override);
our $hooks = {};

sub run_file {
    my @args = @_;
    if (@args && $args[0] eq __PACKAGE__) {
        shift @args;
    }
    my $path = shift @args;
    my $opts = shift @args;
    if (ref($path) eq 'HASH') {
        $opts = $path;
        $path = $opts->{path};
    }
    die "usage: Eval::run_file(<file.fth>)\n" unless defined $path;
    $opts ||= {};
    init_state($path, $opts);
    if (my $post_init = $opts->{post_init}) {
        $post_init->();
    }
    push_source($src);
    run_main();
    if (my $post_run = $opts->{post_run}) {
        $post_run->();
    }
    return 1;
}

sub set_hooks {
    my ($new_hooks) = @_;
    $hooks = $new_hooks || {};
}

sub register_host_semantics {
    my ($name, $code) = @_;
    $host_semantics{lc $name} = $code;
    if (my $word = $dict{lc $name}) {
        $word->{host} = $code;
    }
}

sub lookup_word {
    my ($name) = @_;
    return $dict{lc $name};
}

sub has_word {
    my ($name) = @_;
    return exists $dict{lc $name};
}

sub word_for_header {
    my ($hdr) = @_;
    return $hdr_to_word{$hdr};
}

sub force_exit {
    $force_exit = 1;
}

# Tokenizer with \\ line comments, ( ... ) comments, and ." string literals.
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
            if ($c eq '(' && ($i == 0 || $chars[$i-1] =~ /\s/)) {
                push @toks, $buf if length $buf;
                $buf = '';
                $i++;
                while ($i < @chars && $chars[$i] ne ')') {
                    $i++;
                }
                $i++ if $i < @chars && $chars[$i] eq ')';
                next;
            }
            if ($c eq '.' && $i + 1 < @chars && $chars[$i+1] eq '"') {
                push @toks, $buf if length $buf;
                $buf = '';
                $i  += 2;
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
                $i++;
                next;
            }
            $buf .= $c;
            $i++;
        }
        push @toks, $buf if length $buf;
    }
    return @toks;
}

sub tokenize_file {
    my ($path) = @_;
    open my $handle, '<', $path or die "cannot open $path: $!";
    my @toks = tokenize($handle);
    close $handle;
    return @toks;
}

# Source stack for INCLUDE support.
sub current_source_dir {
    return $source_stack[-1]->{dir} if @source_stack;
    return dirname(File::Spec->rel2abs($src));
}

sub push_source {
    my ($name) = @_;
    my $base = current_source_dir();
    my $path = File::Spec->file_name_is_absolute($name)
      ? $name
      : File::Spec->catfile($base, $name);
    my $abs = File::Spec->rel2abs($path);
    my @toks = tokenize_file($abs);
    push @source_stack, {
        path   => $abs,
        dir    => dirname($abs),
        tokens => \@toks,
        index  => 0,
    };
}

sub next_token {
    while (@source_stack) {
        my $src = $source_stack[-1];
        if ($src->{index} >= @{ $src->{tokens} }) {
            pop @source_stack;
            next;
        }
        return $src->{tokens}[ $src->{index}++ ];
    }
    return undef;
}

# Data and return stacks.
sub push_ds { push @ds, @_; }
sub pop_ds  {
    return pop @ds if @ds;
    my $tok = defined $last_token ? $last_token : '';
    die $tok ne '' ? "data stack underflow near '$tok'" : "data stack underflow";
}
sub push_rs { push @rs, @_; }
sub pop_rs  { pop @rs // die "return stack underflow"; }
sub peek_rs { $rs[-1] // die "return stack underflow"; }

# Runtime state (used by bounds checks and execution).
sub bounds_violation {
    $halt = 1;
    $ip = undef;
    return 0;
}

sub mem_index {
    my ($addr, $size) = @_;
    return undef if $addr < $mem_base;
    my $end = $addr + $size;
    return undef if $end > ($mem_base + $mem_size);
    return $addr - $mem_base;
}

sub read_byte {
    my ($addr) = @_;
    my $idx = mem_index($addr, 1);
    return bounds_violation() unless defined $idx;
    return $mem[$idx] // 0;
}

sub write_byte {
    my ($addr, $val) = @_;
    my $idx = mem_index($addr, 1);
    return bounds_violation() unless defined $idx;
    for my $k (keys %cell_override) {
        delete $cell_override{$k} if $addr >= $k && $addr < $k + 8;
    }
    $mem[$idx] = $val & 0xFF;
    return 1;
}

sub read_cell {
    my ($addr) = @_;
    my $idx = mem_index($addr, 8);
    return bounds_violation() unless defined $idx;
    return $cell_override{$addr} if exists $cell_override{$addr};
    my $bytes = '';
    for my $i (0 .. 7) {
        $bytes .= chr($mem[$idx + $i] // 0);
    }
    return unpack('q<', $bytes);
}

sub write_cell {
    my ($addr, $val) = @_;
    my $idx = mem_index($addr, 8);
    return bounds_violation() unless defined $idx;
    for my $k (keys %cell_override) {
        delete $cell_override{$k} if $k >= $addr && $k < $addr + 8;
    }
    if (ref($val)) {
        $cell_override{$addr} = $val;
        for my $i (0 .. 7) {
            $mem[$idx + $i] = 0;
        }
        return 1;
    }
    delete $cell_override{$addr};
    my $bytes = pack('q<', $val);
    for my $i (0 .. 7) {
        $mem[$idx + $i] = ord(substr($bytes, $i, 1));
    }
    return 1;
}

sub align_here {
    my ($n) = @_;
    while ($here % $n) {
        write_byte($here, 0);
        $here++;
    }
}

sub sync_here {
    my $dh = read_cell($addr{dict_here});
    $here = $dh if $dh > $here;
}

sub alloc_cell {
    my ($name, $init) = @_;
    align_here(8);
    my $addr = $here;
    write_cell($addr, $init // 0);
    $here += 8;
    return $addr;
}

sub alloc_bytes {
    my ($name, $size) = @_;
    my $addr = $here;
    for (1 .. $size) {
        write_byte($here, 0);
        $here++;
    }
    return $addr;
}

sub cell_wrap {
    my ($v) = @_;
    return $v if ref($v);
    return unpack('q<', pack('q<', $v));
}

# Dictionary and XT map.
sub add_word {
    my ($word) = @_;
    my $lname = lc $word->{name};
    if (my $host = $host_semantics{$lname}) {
        $word->{host} = $host;
    }
    $dict{$lname} = $word;
    $xt_to_word{$word->{xt}} = $word if defined $word->{xt};
    $hdr_to_word{$word->{header}} = $word if defined $word->{header};
    $current_word = $word;
}

sub call_hook {
    my ($name, @args) = @_;
    return unless $hooks && $hooks->{$name};
    $hooks->{$name}->(@args);
}

sub create_header {
    my ($name, $immediate) = @_;
    sync_here();
    align_here(8);
    my $hdr = $here;
    write_cell($hdr, $latest_header);
    my $flags = $immediate ? 1 : 0;
    write_byte($hdr + 8, $flags);
    write_byte($hdr + 9, length($name));
    for my $i (0 .. length($name) - 1) {
        write_byte($hdr + 10 + $i, ord(substr($name, $i, 1)));
    }
    $here = $hdr + 10 + length($name);
    align_here(8);
    my $cfa = $here;
    write_cell($cfa, 0);
    $here = $cfa + 8;
    $latest_header = $hdr;
    write_cell($addr{latest}, $latest_header);
    write_cell($addr{dict_here}, $here);
    return ($hdr, $cfa);
}

sub add_primitive {
    my ($name, $code, $immediate) = @_;
    my ($hdr, $cfa) = create_header($name, $immediate);
    my $word = {
        name      => $name,
        kind      => 'prim',
        prim      => $code,
        immediate => $immediate ? 1 : 0,
        xt        => $cfa,
        header    => $hdr,
        pfa       => $cfa + 8,
    };
    add_word($word);
    call_hook('add_primitive', $word);
    return $word;
}

sub start_colon_word {
    my ($name) = @_;
    my ($hdr, $cfa) = create_header($name, 0);
    my $word = {
        name      => $name,
        kind      => 'colon',
        immediate => 0,
        xt        => $cfa,
        header    => $hdr,
        pfa       => $cfa + 8,
    };
    add_word($word);
    call_hook('start_colon', $word);
    $compile_state = 1;
    %cur_labels = ();
    %cur_label_refs = ();
    @cstack = ();
}

sub finish_colon_word {
    die "no current word" unless $current_word;
    my $exit_word = $dict{'exit'} or die "missing exit primitive";
    compile_xt($exit_word);
    for my $name (keys %cur_label_refs) {
        die "unresolved label $name";
    }
    call_hook('finish_colon', $current_word);
    $compile_state = 0;
    $current_word = undef;
    %cur_labels = ();
    %cur_label_refs = ();
    @cstack = ();
}

sub compile_cell {
    my ($val) = @_;
    my $addr = read_cell($addr{dict_here});
    write_cell($addr, $val);
    write_cell($addr{dict_here}, $addr + 8);
    sync_here();
}

sub compile_xt {
    my ($word) = @_;
    compile_cell($word->{xt});
    call_hook('compile_xt', $word);
}

sub compile_lit {
    my ($val, $meta) = @_;
    my $lit_word = $dict{'lit'} or die "missing lit primitive";
    compile_xt($lit_word);
    compile_cell($val);
    call_hook('compile_lit', $val, $meta);
}

sub compile_string {
    my ($text) = @_;
    $data_here = $here if $data_here < $here;
    my $addr = $data_here;
    for my $i (0 .. length($text) - 1) {
        write_byte($data_here++, ord(substr($text, $i, 1)));
    }
    call_hook('string_literal', $text);
    compile_lit($addr, { kind => 'string_addr', text => $text });
    compile_lit(length($text), { kind => 'string_len', text => $text });
    my $type_word = $dict{'type'} or die "missing type primitive";
    compile_xt($type_word);
}

sub is_immediate {
    my ($word) = @_;
    return 1 if $word->{immediate};
    if (defined $word->{header}) {
        my $flags = read_byte($word->{header} + 8);
        return ($flags & 1) ? 1 : 0;
    }
    return 0;
}

sub run_xt {
    my ($xt) = @_;
    my $word = $xt_to_word{$xt} or die "unknown xt $xt";
    if ($word->{host}) {
        $word->{host}->($word);
        return;
    }
    if ($word->{kind} eq 'prim') {
        $word->{prim}->();
        return;
    }
    if ($word->{kind} eq 'colon') {
        push_rs($ip) if defined $ip;
        $ip = $word->{pfa};
        return;
    }
    die "unknown word kind '$word->{kind}'";
}

sub run_thread {
    my ($xt) = @_;
    local $ip = undef;
    run_xt($xt);
    if ($force_exit) {
        $force_exit = 0;
        return;
    }
    while (defined $ip) {
        my $next_xt = read_cell($ip);
        $ip += 8;
        run_xt($next_xt);
        last if $halt;
        if ($force_exit) {
            $force_exit = 0;
            last;
        }
    }
}

sub run_thread_at {
    my ($addr) = @_;
    local $ip = $addr;
    while (defined $ip) {
        my $next_xt = read_cell($ip);
        $ip += 8;
        run_xt($next_xt);
        last if $halt;
        if ($force_exit) {
            $force_exit = 0;
            last;
        }
    }
}

sub token_to_filename {
    my ($tok) = @_;
    die "include missing filename" unless defined $tok;
    if (ref($tok) eq 'HASH' && $tok->{type} && $tok->{type} eq 'string') {
        return $tok->{text};
    }
    return $tok;
}

sub exec_word {
    my ($word) = @_;
    if ($word->{host}) {
        $word->{host}->($word);
        return;
    }
    if ($word->{kind} eq 'prim') {
        $word->{prim}->();
        return;
    }
    if ($word->{kind} eq 'colon') {
        run_thread($word->{xt});
        return;
    }
    die "unknown word kind '$word->{kind}'";
}

sub compile_if {
    my $zb = $dict{'0branch'} or die "missing 0branch";
    compile_xt($zb);
    my $addr = read_cell($addr{dict_here});
    compile_cell(0);
    push @cstack, { type => 'if', addr => $addr };
}

sub compile_else {
    my $ent = pop @cstack // die "else without if";
    die "else without if" unless $ent->{type} eq 'if';
    my $br = $dict{'branch'} or die "missing branch";
    compile_xt($br);
    my $addr = read_cell($addr{dict_here});
    compile_cell(0);
    write_cell($ent->{addr}, read_cell($addr{dict_here}));
    push @cstack, { type => 'if', addr => $addr };
}

sub compile_then {
    my $ent = pop @cstack // die "then without if";
    die "then without if" unless $ent->{type} eq 'if';
    write_cell($ent->{addr}, read_cell($addr{dict_here}));
}

sub init_primitives {
    # Primitive words.
    add_primitive('lit', sub {
        my $val = read_cell($ip);
        $ip += 8;
        push_ds($val);
    }, 0);

    add_primitive('exit', sub {
        if (!@rs) {
            $ip = undef;
            return;
        }
        $ip = pop_rs();
    }, 0);

    add_primitive('branch', sub {
        my $target = read_cell($ip);
        $ip = $target;
    }, 0);

    add_primitive('0branch', sub {
        my $v = pop_ds();
        my $target = read_cell($ip);
        $ip += 8;
        if ($v == 0) {
            $ip = $target;
        }
    }, 0);

    add_primitive('execute', sub {
        my $xt = pop_ds();
        if (defined $ip) {
            run_xt($xt);
        } else {
            run_thread($xt);
        }
    }, 0);

    add_primitive('dodoes', sub {
        die "dodoes not supported in eval";
    }, 0);

    add_primitive('do-does', sub {
        pop_ds();
        $force_exit = 1;
    }, 0);

    add_primitive('does>', sub {
        die "does> outside compile" unless $compile_state;
        my $lit_word = $dict{'lit'} or die "missing lit primitive";
        compile_xt($lit_word);
        my $lit_addr = read_cell($addr{dict_here});
        compile_cell(0);
        my $do_word = $dict{'do-does'} or die "missing do-does primitive";
        compile_xt($do_word);
        my $does_addr = read_cell($addr{dict_here});
        write_cell($lit_addr, $does_addr);
    }, 1);

    add_primitive('bye', sub { $halt = 1; $ip = undef; }, 0);
    add_primitive('emit', sub { my $v = pop_ds(); print chr($v & 0xFF); }, 0);
    add_primitive('cr', sub { print "\n"; }, 0);
    add_primitive('space', sub { print " "; }, 0);
    add_primitive('.', sub { my $v = pop_ds(); print $v; }, 0);
    add_primitive('dup', sub { my $v = $ds[-1] // die "dup underflow"; push @ds, $v; }, 0);
    add_primitive('drop', sub { pop @ds if @ds; }, 0);
    add_primitive('swap', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $a, $b; }, 0);
    add_primitive('over', sub { my $n = @ds; die "over underflow" if $n < 2; push @ds, $ds[$n-2]; }, 0);
    add_primitive('rot', sub { my $a = pop_ds(); my $b = pop_ds(); my $c = pop_ds(); push @ds, $b, $a, $c; }, 0);
    add_primitive('-rot', sub { my $a = pop_ds(); my $b = pop_ds(); my $c = pop_ds(); push @ds, $a, $c, $b; }, 0);
    add_primitive('2dup', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $b, $a, $b, $a; }, 0);
    add_primitive('2drop', sub { pop_ds(); pop_ds(); }, 0);
    add_primitive('>r', sub { push_rs(pop_ds()); }, 0);
    add_primitive('r>', sub { push_ds(pop_rs()); }, 0);
    add_primitive('r@', sub { push_ds(peek_rs()); }, 0);
    add_primitive('rdrop', sub { pop_rs(); }, 0);
    add_primitive('+', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $a + $b; }, 0);
    add_primitive('-', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $b - $a; }, 0);
    add_primitive('*', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $a * $b; }, 0);
    add_primitive('/', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, int($b / $a); }, 0);
    add_primitive('mod', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $b % $a; }, 0);
    add_primitive('2/', sub { my $a = pop_ds(); push @ds, $a >> 1; }, 0);
    add_primitive('and', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $a & $b; }, 0);
    add_primitive('or', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $a | $b; }, 0);
    add_primitive('xor', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, $a ^ $b; }, 0);
    add_primitive('invert', sub { my $a = pop_ds(); push @ds, cell_wrap(~$a); }, 0);
    add_primitive('=', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, ($a == $b) ? -1 : 0; }, 0);
    add_primitive('<', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, ($b < $a) ? -1 : 0; }, 0);
    add_primitive('>', sub { my $a = pop_ds(); my $b = pop_ds(); push @ds, ($b > $a) ? -1 : 0; }, 0);
    add_primitive('0=', sub { my $a = pop_ds(); push @ds, ($a == 0) ? -1 : 0; }, 0);
    add_primitive('0<', sub { my $a = pop_ds(); push @ds, ($a < 0) ? -1 : 0; }, 0);
    add_primitive('1+', sub { my $a = pop_ds(); push @ds, $a + 1; }, 0);
    add_primitive('1-', sub { my $a = pop_ds(); push @ds, $a - 1; }, 0);
    add_primitive('here', sub { push_ds(read_cell($addr{dict_here})); }, 0);
    add_primitive('allot', sub { my $n = pop_ds(); write_cell($addr{dict_here}, read_cell($addr{dict_here}) + $n); sync_here(); }, 0);
    add_primitive('align-here', sub {
        my $v = read_cell($addr{dict_here});
        $v = ($v + 7) & ~7;
        write_cell($addr{dict_here}, $v);
        sync_here();
    }, 0);
    add_primitive(',', sub { my $v = pop_ds(); compile_cell($v); }, 0);
    add_primitive('c,', sub { my $v = pop_ds(); my $addr = read_cell($addr{dict_here}); write_byte($addr, $v); write_cell($addr{dict_here}, $addr + 1); sync_here(); }, 0);
    add_primitive('@', sub { my $addr = pop_ds(); push_ds(read_cell($addr)); }, 0);
    add_primitive('!', sub { my $addr = pop_ds(); my $v = pop_ds(); write_cell($addr, $v); }, 0);
    add_primitive('c@', sub { my $addr = pop_ds(); push_ds(read_byte($addr)); }, 0);
    add_primitive('c!', sub { my $addr = pop_ds(); my $v = pop_ds(); write_byte($addr, $v); }, 0);
    add_primitive('+!', sub { my $addr = pop_ds(); my $v = pop_ds(); write_cell($addr, read_cell($addr) + $v); }, 0);
    add_primitive('move', sub {
        my $count = pop_ds();
        my $dest = pop_ds();
        my $src = pop_ds();
        return if $count <= 0;
        if ($dest > $src && $dest < $src + $count) {
            for (my $i = $count - 1; $i >= 0; $i--) {
                write_byte($dest + $i, read_byte($src + $i));
            }
        } else {
            for (my $i = 0; $i < $count; $i++) {
                write_byte($dest + $i, read_byte($src + $i));
            }
        }
    }, 0);
    add_primitive('fill', sub {
        my $char = pop_ds();
        my $count = pop_ds();
        my $addr = pop_ds();
        return if $count <= 0;
        for (my $i = 0; $i < $count; $i++) {
            write_byte($addr + $i, $char);
        }
    }, 0);
    add_primitive('type', sub {
        my $len = pop_ds();
        my $addr = pop_ds();
        return if $len <= 0;
        my $out = '';
        for (my $i = 0; $i < $len; $i++) {
            $out .= chr(read_byte($addr + $i));
        }
        print $out;
    }, 0);
    add_primitive('key', sub {
        my $buf = '';
        my $n = sysread(STDIN, $buf, 1);
        die "key: no input" if !defined $n || $n == 0;
        push_ds(ord($buf));
    }, 0);
    add_primitive('next-token', sub {
        my $tok = next_token();
        if (!defined $tok) {
            push_ds(0);
            push_ds(0);
            push_ds(0);
            return;
        }
        if (ref($tok) eq 'HASH') {
            die "next-token: unexpected token" unless $tok->{type} && $tok->{type} eq 'string';
            $tok = $tok->{text};
        }
        my $text = $tok;
        my $len = length($text);
        die "next-token too long" if $len > 255;
        my $buf = $addr{input_buf};
        for my $i (0 .. $len - 1) {
            write_byte($buf + $i, ord(substr($text, $i, 1)));
        }
        write_cell($addr{input_len}, $len);
        write_cell($addr{input_pos}, 0);
        push_ds($buf);
        push_ds($len);
        push_ds(-1);
    }, 0);
    add_primitive('include', sub {
        my $tok = next_token();
        my $name = token_to_filename($tok);
        push_source($name);
    }, 1);
add_primitive('immediate', sub {
    my $hdr = read_cell($addr{latest});
    die "immediate: no latest word" if $hdr == 0;
    my $flags = read_byte($hdr + 8);
    write_byte($hdr + 8, $flags | 1);
    call_hook('immediate', $hdr_to_word{$hdr}, $hdr);
}, 0);

    add_primitive('find-name', sub {
        my $len = pop_ds();
        my $addr = pop_ds();
        my $hdr = read_cell($addr{latest});
        my $limit = 256;
        while ($hdr != 0 && $limit-- > 0) {
            my $flags = read_byte($hdr + 8);
            my $nlen = read_byte($hdr + 9);
            if ($nlen == $len) {
                my $match = 1;
                for (my $i = 0; $i < $nlen; $i++) {
                    if (read_byte($addr + $i) != read_byte($hdr + 10 + $i)) {
                        $match = 0;
                        last;
                    }
                }
                if ($match) {
                    my $cfa = $hdr + 10 + $nlen;
                    $cfa = ($cfa + 7) & ~7;
                    push_ds($cfa);
                    push_ds(($flags & 1) ? 1 : -1);
                    return;
                }
            }
            $hdr = read_cell($hdr);
        }
        push_ds(0);
        push_ds(0);
    }, 0);

    # Address constants for &xt_* names.
    $addr{xt_branch} = $dict{'branch'}->{xt};
    $addr{xt_zero_branch} = $dict{'0branch'}->{xt};
    $addr{xt_forth_branch} = $dict{'branch'}->{xt};
    $addr{xt_forth_zero_branch} = $dict{'0branch'}->{xt};
    $addr{xt_forth_exit} = $dict{'exit'}->{xt};
    $addr{xt_forth_lit} = $dict{'lit'}->{xt};

    # Compile-time control words (host side, optional).
    add_primitive('if', sub { die "if outside compile" unless $compile_state; compile_if(); }, 1);
    add_primitive('else', sub { die "else outside compile" unless $compile_state; compile_else(); }, 1);
    add_primitive('then', sub { die "then outside compile" unless $compile_state; compile_then(); }, 1);
    add_primitive('end', sub { die "end outside compile" unless $compile_state; compile_then(); }, 1);
    add_primitive('begin', sub {
        die "begin outside compile" unless $compile_state;
        push @cstack, { type => 'begin', addr => read_cell($addr{dict_here}) };
    }, 1);
    add_primitive('again', sub {
        die "again outside compile" unless $compile_state;
        my $ent = pop @cstack // die "again without begin";
        die "again without begin" unless $ent->{type} eq 'begin';
        my $br = $dict{'branch'} or die "missing branch";
        compile_xt($br);
        compile_cell($ent->{addr});
    }, 1);
    add_primitive('until', sub {
        die "until outside compile" unless $compile_state;
        my $ent = pop @cstack // die "until without begin";
        die "until without begin" unless $ent->{type} eq 'begin';
        my $zb = $dict{'0branch'} or die "missing 0branch";
        compile_xt($zb);
        compile_cell($ent->{addr});
    }, 1);
    add_primitive('while', sub {
        die "while outside compile" unless $compile_state;
        my $ent = $cstack[-1] // die "while without begin";
        die "while without begin" unless $ent->{type} eq 'begin';
        my $zb = $dict{'0branch'} or die "missing 0branch";
        compile_xt($zb);
        my $addr = read_cell($addr{dict_here});
        compile_cell(0);
        push @cstack, { type => 'if', addr => $addr, kind => 'while' };
    }, 1);
    add_primitive('repeat', sub {
        die "repeat outside compile" unless $compile_state;
        my $if_ent = pop @cstack // die "repeat without while";
        die "repeat without while" unless $if_ent->{type} eq 'if' && $if_ent->{kind} && $if_ent->{kind} eq 'while';
        my $begin_ent = pop @cstack // die "repeat without begin";
        die "repeat without begin" unless $begin_ent->{type} eq 'begin';
        my $br = $dict{'branch'} or die "missing branch";
        compile_xt($br);
        compile_cell($begin_ent->{addr});
        write_cell($if_ent->{addr}, read_cell($addr{dict_here}));
    }, 1);
}

sub run_main {
TOKEN: while (defined(my $tok = next_token())) {
        if (ref($tok) eq 'HASH' && $tok->{type} && $tok->{type} eq 'string') {
            $last_token = '."'.$tok->{text}.'"';
        } else {
            $last_token = $tok;
        }
        if (ref($tok) eq 'HASH' && $tok->{type} && $tok->{type} eq 'string') {
            if ($compile_state) {
                compile_string($tok->{text});
            } else {
                print $tok->{text};
            }
            next TOKEN;
        }

        if ($tok eq ':') {
            my $name = next_token() // die "':' without name";
            start_colon_word($name);
            next TOKEN;
        }

        if ($tok eq ';') {
            die "';' without matching ':'" unless $compile_state;
            finish_colon_word();
            next TOKEN;
        }

    if ($tok =~ /^&([A-Za-z_]\w*)$/) {
        my $name = $1;
        my $resolved;
        if ($hooks && $hooks->{resolve_addr}) {
            $resolved = $hooks->{resolve_addr}->($name);
        } else {
            $resolved = $addr{$name};
        }
        die "unknown &label '$name'" unless defined $resolved;
        if ($compile_state) {
            compile_lit($resolved, { kind => 'addr', name => $name });
        } else {
            push_ds($resolved);
        }
        next TOKEN;
    }

    if ($tok =~ /^label:([A-Za-z_]\w*)$/) {
        die "label outside compile" unless $compile_state;
        my $name = $1;
        $cur_labels{$name} = read_cell($addr{dict_here});
        if (my $refs = $cur_label_refs{$name}) {
            for my $addr (@$refs) {
                write_cell($addr, $cur_labels{$name});
            }
            delete $cur_label_refs{$name};
        }
        call_hook('label_def', $name);
        next TOKEN;
    }

    if ($tok =~ /^>([A-Za-z_]\w*)$/ && $tok ne '>r') {
        die "label ref outside compile" unless $compile_state;
        my $name = $1;
        my $addr = read_cell($addr{dict_here});
        if (exists $cur_labels{$name}) {
            compile_cell($cur_labels{$name});
        } else {
            compile_cell(0);
            push @{ $cur_label_refs{$name} }, $addr;
        }
        call_hook('label_ref', $name);
        next TOKEN;
    }

    if (looks_like_number($tok) && $tok =~ /^-?\d+$/) {
        if ($compile_state) {
            compile_lit($tok + 0, { kind => 'number', text => $tok });
        } else {
            push_ds($tok + 0);
        }
        next TOKEN;
    }

    my $word = $dict{lc $tok} or die "unknown word '$tok'";
    if ($compile_state) {
        if (is_immediate($word)) {
            exec_word($word);
        } else {
            compile_xt($word);
        }
    } else {
        exec_word($word);
    }
        last TOKEN if $halt;
    }

    die "unterminated definition at EOF" if $compile_state;

    if (!$halt && $dict{'boot'}) {
        run_thread($dict{'boot'}->{xt});
    }
}

sub init_state {
    my ($path, $opts) = @_;
    $opts ||= {};
    $src = $path;
    @source_stack = ();
    @ds = ();
    @rs = ();
    $halt = 0;
    $ip = undef;
    $force_exit = 0;
    @mem = ();
    %cell_override = ();
    $mem_base = 0x40000000;
    $mem_size = 0x08000000;
    $here = $mem_base;
    $data_here = $mem_base + 0x01000000;
    %addr = ();
    %dict = ();
    %xt_to_word = ();
    %hdr_to_word = ();
    $latest_header = 0;
    $compile_state = 0;
    $current_word = undef;
    %cur_labels = ();
    %cur_label_refs = ();
    @cstack = ();
    $hooks = $opts->{hooks} || {};

    # Allocate core variables.
    $addr{var_cell}  = alloc_cell('var_cell', 0);
    $addr{var_byte}  = alloc_bytes('var_byte', 1);
    $addr{var_pair}  = alloc_bytes('var_pair', 16);
    $addr{input_buf} = alloc_bytes('input_buf', 256);
    $addr{input_len} = alloc_cell('input_len', 0);
    $addr{input_pos} = alloc_cell('input_pos', 0);
    $addr{dict_here} = alloc_cell('dict_here', 0);
    $addr{latest}    = alloc_cell('latest', 0);

    # dict_here starts at current here.
    write_cell($addr{dict_here}, $here);

    init_primitives();
    $addr{forth_dodoes} = $dict{'dodoes'}->{xt} if exists $dict{'dodoes'};
}

1;
