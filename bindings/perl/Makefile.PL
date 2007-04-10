use ExtUtils::MakeMaker;

use Config;
use File::Copy 'cp';
use lib qw(lib);
use Sigar::ArchName ();

my $installdir;
if ($ARGV[0]) {
    $installdir = $ARGV[0];
}
else {
    #check binary-dist location and svn source location
    for (qw(../../sigar-bin ../java/sigar-bin)) {
        if (-e $_) {
            $installdir = $_;
            last;
        }
    }
}

my $generated_xs = "Sigar_generated.xs";
my $build_xs = "../java/build/src/$generated_xs";
if (-e $build_xs) {
    #XXX should be done in the generated Makefile
    if ((stat $generated_xs)[9] < (stat $build_xs)[9]) {
        print "Updating $generated_xs from $build_xs\n";
        cp $build_xs, $generated_xs;
    }
}

my $archname = Sigar::ArchName->get_name();

my $ccname = $Config{ccname};

my $define = {
    gcc => '-Wall -Werror',
    cc_r => '-qhalt=w',
}->{$ccname} || '';

WriteMakefile(
    'NAME'		=> 'Sigar',
    'VERSION_FROM'	=> 'Sigar.pm',
    'LIBS'		=> ["-L$installdir/lib -lsigar-$archname"],
    'INC'		=> "-I$installdir/include",
    'DEFINE'            => $define,
    'depend'            => { 'Sigar.c' => 'Sigar_generated.xs' },
);
