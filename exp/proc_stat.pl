#!perl

use strict;
#examine specific field from /proc/*/stat
my $field = shift;

my $proc = "/proc";
opendir DH, $proc or die;
chdir $proc;
local $/;

while (my $pid = readdir DH) {
    next unless $pid =~ /^\d+$/;
    open FH, "$pid/stat" or next;
    my $data = <FH>;
    close FH;
    my(@fields) = split /\s+/, $data;
    print "$pid $fields[1] -> $fields[$field]\n";
}

closedir DH;
