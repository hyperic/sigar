package Sigar::ArchName;

use strict;
use Config;

sub get_name {
    my $os = lc $^O;
    my $vers = $Config{osvers};
    my $arch = $Config{archname};

    if ($os =~ /win32/) {
        return 'x86-winnt';
    }
    elsif ($os =~ /linux/) {
        return 'x86-linux';
    }
    elsif ($os =~ /hpux/) {
        if ($vers =~ /11\./) {
            return 'pa-hpux-11';
        }
    }
    elsif ($os =~ /aix/) {
        return 'ppc-aix-5';
    }
    elsif ($os =~ /solaris/) {
        if ($arch =~ /sun4/) {
            return 'sparc-solaris';
        }
        elsif ($arch =~ /.86/) {
            return 'x86-solaris';
        }
    }
    elsif ($os =~ /darwin/) {
        return 'universal-macosx';
    }

    die "Unsupported platform";
}

1;
__END__
