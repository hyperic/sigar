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
            return 'hppa2.0-hp-hpux-11.x';
        }
    }
    elsif ($os =~ /aix/) {
        return 'powerpc-ibm-aix-4.3.x';
    }
    elsif ($os =~ /solaris/) {
        if ($arch =~ /sun4/) {
            return 'sparc-sun-solaris-2.x';
        }
        elsif ($arch =~ /.86/) {
            return 'x86-sun-solaris-2.x';
        }
    }
    elsif ($os =~ /darwin/) {
        return 'powerpc-apple-darwin';
    }

    die "Unsupported platform";
}

1;
__END__
