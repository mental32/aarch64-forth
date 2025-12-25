#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib $FindBin::Bin;
use Eval;

my $path = shift(@ARGV) // die "usage: eval.pl <file.fth>\n";
Eval::run_file($path);
