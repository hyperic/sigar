#!/usr/bin/perl

#generate a process with large argv for proc_args testing

use strict;

if (@ARGV) {
    print "pid=$$\n";
    <STDIN>;
}
else {
    my(@argv);
    my $nargs = 126;
    my $arglen = 256;
    my $arg = 'a';
    for (my $i=0; $i<$nargs; $i++) {
	push @argv, $arg++ x $arglen;
    }

    exec $^X, $0, @argv;
} 
