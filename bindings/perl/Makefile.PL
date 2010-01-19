use ExtUtils::MakeMaker;

use strict;
use Config;
use File::Basename qw(basename);
use File::Copy qw(copy);
use lib qw(.. lib);
use SigarWrapper ();
use SigarBuild ();

my $gen_xs = 'Sigar_generated.xs';
my(@mm_args);
my(@clean_files) = ($gen_xs);

if ($ARGV[0]) {
    #link against installed libsigar
    my $installdir = $ARGV[0];
    @mm_args = (
        LIBS => ["-L$installdir/lib -lsigar"],
        INC  => "-I$installdir/include",
    );
}
else {
    #all sources in Sigar.so
    my $flags = SigarBuild::flags();
    my(@inline_src) = SigarBuild::inline_src($flags);
    push @clean_files, @inline_src;
    my(@object) = ('Sigar.o', map { s/cp{0,2}$/o/; $_ } @inline_src);
    my(@libs) = map { "-l$_" }  @{$flags->{libs}};

    @mm_args = (
        OBJECT => "@object",
        LIBS => ["@libs"],
        INC => "@{$flags->{cppflags}}",
        dynamic_lib => { OTHERLDFLAGS => "@{$flags->{ldflags}}" },
    );
}

SigarWrapper::generate(Perl => '.');

my $ccname = $Config{ccname};

my $define = {
    gcc => '-Wall',
    cc_r => '-qhalt=w',
}->{$ccname} || '';

WriteMakefile(
    'NAME'		=> 'Sigar',
    'VERSION_FROM'	=> 'Sigar.pm',
    'DEFINE'            => $define,
    'depend'            => { 'Sigar.c' => $gen_xs },
    'realclean'         => { FILES => "@clean_files" },
    @mm_args,
);
