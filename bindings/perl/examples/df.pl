#!/usr/bin/perl

use strict;
use Sigar;

my $sigar = new Sigar;

my $fslist = $sigar->file_system_list;

print "Filesystem\tSize\tUsed\tAvail\tUse%\tMounted on\tType\n";

for my $fs (@$fslist) {
    my $dirname = $fs->dir_name;
    my $usage;

    #e.g. on win32 D:\ fails with "Device not ready"
    #if there is no cd in the drive.
    eval {
        $usage = $sigar->file_system_usage($dirname);
    } or next;

    my $total = $usage->total;
    my $used  = $total - $usage->free;
    my $avail = $usage->avail;
    my $pct   = $usage->use_percent * 100;
    my $usePct;

    if ($pct == 0) {
        $usePct = "-"
    }
    else {
        $usePct = $pct . '%';
    }

    print
      $fs->dev_name . "\t" .
      format_size($total) . "\t" .
      format_size($used) . "\t" .
      format_size($avail) . "\t" .
      $usePct . "\t" .
      $dirname . "\t" .
      $fs->sys_type_name . "/" . $fs->type_name . "\n";

}

sub format_size {
    my($size) = @_;
    return Sigar::format_size($size * 1024);
}
