use ExtUtils::MakeMaker;

use Config;
use lib qw(lib);
use Sigar::ArchName ();

my $installdir = $ARGV[0] || "../../sigar-bin";
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
