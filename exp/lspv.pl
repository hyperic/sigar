#!/usr/bin/perl

use strict;

my $lspv = "/usr/sbin/lspv";

open LSPV, "$lspv|" or die;
my(@lspv) = <LSPV>;
close LSPV;

my $dlm = ("-" x 25) . "\n";

print "$lspv\n", @lspv;
print $dlm;

for my $line (@lspv) {
    my $disk = (split /\s+/, $line)[0];
    next unless $disk;
    next if $line =~ / None/;
    my $cmd = "$lspv -l $disk";
    open LSPV, "$cmd|" or die;
    my(@pv) = <LSPV>;
    close LSPV;
    print "$cmd\n", @pv;
    print $dlm;
}
