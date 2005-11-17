#!/usr/bin/perl

#generate a process with large argv for proc_args testing

use strict;

if (@ARGV) {
    print "pid=$$\n";
    <STDIN>;
}
else {
    my(@argv);
    my $nargs = 26;
    my $arglen = 4256;
    my $arg = 'a';
    for (my $i=0; $i<$nargs; $i++) {
	push @argv, $arg++ x $arglen;
    }
    print "$^X $0\n";
    exec $^X, $0, @argv or die $!;
} 
