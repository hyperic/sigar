#!/usr/bin/perl

use strict;
use Sigar;

my $sigar = new Sigar;

my $infos = $sigar->cpu_info_list;

my $num = scalar @$infos;

print "$num total CPUs..\n";

for my $info (@$infos) {
    print "Vendor........" . $info->vendor . "\n";
    print "Model........." . $info->model . "\n";
    print "Mhz..........." . $info->mhz . "\n";
    print "Cache size...." . $info->cache_size . "\n";
}
