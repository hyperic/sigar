#!/usr/bin/perl

use strict;
use Sigar;

my $sigar = new Sigar;

my $mem = $sigar->mem;

my $swap = $sigar->swap;

print "\tTotal\tUsed\tFree\n";

print "Mem:    " .
      ($mem->total / 1024) . "\t" .
      ($mem->used / 1024) . "\t" .
      ($mem->free / 1024) . "\n";

print "Swap:   " .
      ($swap->total / 1024) . "\t" .
      ($swap->used / 1024) . "\t" .
      ($swap->free / 1024) . "\n";

print "RAM:    " . $mem->ram . "MB\n";
