#!perl

#create a .tar.gz of /proc/*/@proc_files

use strict;

my(@proc_files) = qw(cmdline stat statm status);
my $cpname = "copy-proc";
my $tmp = $ENV{TMPDIR} || "/tmp";
my $tmpdir = "$tmp/$cpname";
unless (-e $tmpdir) {
    mkdir $tmpdir or die "mkdir $tmpdir: $!";
    print "created $tmpdir\n";
}

opendir DH, "/proc" or die;
chdir "/proc";

while (my $pid = readdir DH) {
    next unless $pid =~ /^\d+$/;
    for my $name (@proc_files) {
	unless (-e "$tmpdir/$pid") {
	    mkdir "$tmpdir/$pid";
	}
	local *SRC, *TARG;
	my $file = "$pid/$name";
	open SRC, $file or next;
	open TARG, ">$tmpdir/$file" or die "open $tmpdir/$file: $!";
	print TARG <SRC>;
	close TARG;
	close SRC;
    }
}

chdir $tmp or die "chdir $tmp: $!";
system "tar -cf $cpname.tar $cpname";
system "gzip -f $cpname.tar";

print "files saved to $tmp/$cpname.tar.gz\n";
print "to cleanup, run: rm -rf $tmp/$cpname\n";


