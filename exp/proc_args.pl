#!perl

use strict;

my $match = $ARGV[0];

opendir DH, "/proc" or die;
chdir "/proc";
local $/;

while (my $pid = readdir DH) {
    next unless $pid =~ /^\d+$/;
    open FH, "$pid/cmdline" or next;
    my(@cmdline) = split /\000/, <FH>;
    close FH;
    if ($match) {
	next unless grep { /$match/o } @cmdline;
    }
    print "$pid=[", (map { "=>$_<=" } @cmdline), "]\n";
}

closedir DH;
