#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename qw(dirname);
use Cwd qw(abs_path);
use IPC::Open3;
use Symbol qw(gensym);

# Move to repo root (this file now lives at repo root)
my $root = abs_path(dirname(__FILE__));
chdir $root or die "cannot chdir to $root: $!";

my $timeout_secs = 1;
my $verbose = $ENV{VERBOSE} ? 1 : 0;

# Fresh zig cache if requested
if (-d ".zig-cache") {
    system('rm', '-rf', '.zig-cache');
}

while (@ARGV) {
    my $arg = shift @ARGV;
    if ($arg eq '--timeout') {
        $timeout_secs = shift(@ARGV) // die "--timeout requires a value";
    } elsif ($arg eq '--verbose') {
        $verbose = 1;
    } else {
        die "Unknown arg: $arg\n";
    }
}

sub expect_for_file {
    my ($path) = @_;
    open my $fh, '<', $path or die "open $path: $!";
    my $expect;
    my $input;
    while (my $line = <$fh>) {
        if ($line =~ /^\s*\\\s*EXPECT:\s*(.*)$/) {
            $expect = $1;
            chomp $expect;
        }
        if ($line =~ /^\s*\\\s*INPUT:\s*(.*)$/) {
            $input = $1;
            $input .= "\n" unless $input =~ /\n\z/;
        }
    }
    close $fh;
    return ($expect, $input);
}

sub which {
    my ($prog) = @_;
    my $out = `PATH="$ENV{PATH}" command -v $prog 2>/dev/null`;
    chomp $out;
    return $out || undef;
}

sub run_cmd {
    my ($cmd, $input) = @_;
    print "cmd=$cmd\n" if $verbose;
    if (defined $input) {
        my $err = gensym;
        my $pid = open3(my $in, my $out, $err, $cmd);
        print {$in} $input;
        close $in;
        my $stdout = do { local $/; <$out> // '' };
        my $stderr = do { local $/; <$err> // '' };
        waitpid($pid, 0);
        my $status = $? >> 8;
        return ($stdout . $stderr, $status);
    } else {
        my $out = `$cmd 2>&1`;
        my $status = $? >> 8;
        return ($out, $status);
    }
}

my $qemu = $ENV{QEMU} || which('qemu-system-aarch64')
  or die "qemu-system-aarch64 not found (set QEMU env var)\n";

my $timeout_bin = which('timeout') || which('gtimeout');

for my $t (sort glob("tests/*.fs")) {
    print "==> $t\n";

    my ($expect, $input) = expect_for_file($t);
    if (!defined $expect || $expect eq '') {
        die "No EXPECT line in $t\n";
    }

    my @clean = ('make', 'clean');
    print "+ @clean\n" if $verbose;
    system(@clean) == 0 or die "make clean failed\n";

    my @build = ('make', "SRC=$t");
    print "+ @build\n" if $verbose;
    system(@build) == 0 or die "make SRC=$t failed\n";

    my $base_cmd = qq{$qemu -machine virt -cpu cortex-a72 -nographic -kernel forth.elf};
    my $cmd = $base_cmd;
    if ($timeout_bin) {
        $cmd = qq{$timeout_bin ${timeout_secs}s $cmd};
    }

    my ($out, $status) = run_cmd($cmd, $input);
    my $out_clean = $out;
    $out_clean =~ s/[\r\n]+//g;

    if ($out_clean ne $expect && $timeout_bin) {
        # Retry without timeout to rule out premature kill
        ($out, $status) = run_cmd($base_cmd, $input);
        $out_clean = $out;
        $out_clean =~ s/[\r\n]+//g;
    }

    if ($out_clean ne $expect) {
        print "FAIL $t: got '$out_clean' expected '$expect'\n";
        if ($verbose) {
            print "raw output:\n$out\n";
            print "exit status: $status\n";
        }
        exit 1;
    } else {
        print "ok\n";
    }
}

exit 0;
