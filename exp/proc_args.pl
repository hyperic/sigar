#!perl

use strict;

my $match = $ARGV[0];

my $proc = "/proc";
opendir DH, $proc or die;
chdir $proc;
local $/;

while (my $pid = readdir DH) {
    next unless $pid =~ /^\d+$/;
    open FH, "$pid/cmdline" or next;
    my(@cmdline) = split /\000/, <FH>;
    close FH;
    if ($match) {
	next unless grep { /$match/o } @cmdline;
    }

    print "-------------------------------\n";

    my $i=0;
    print "pid=$pid\n";
    for my $arg (@cmdline) {
	print "$i='$arg'\n";
	$i++;
    }
}

closedir DH;
