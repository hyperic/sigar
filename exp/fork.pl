#!/usr/bin/perl

use strict;

#create processes to benchmark ptql stuff

my $num = shift || 10;
my @kids;
my $pid;

for (my $i=0; $i<$num; $i++) {
    if (!defined($pid = fork())) {
	die "cannot fork: $!";
    }
    elsif ($pid) {
	#parent
	push @kids, $pid;
    }
    else {
	#child
	sleep 10 while 1;
    }
}

for $pid (@kids) {
    waitpid($pid, 0);
}
