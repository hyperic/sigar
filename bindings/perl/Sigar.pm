package Sigar;

use 5.003;
use strict;
use vars qw($VERSION @ISA);

$VERSION = '0.01';

eval {
    require XSLoader;
};

if ($@) {
    #$] < 5.6
    require DynaLoader;
    @ISA = qw(DynaLoader);
    __PACKAGE__->bootstrap($VERSION);
}
else {
    XSLoader::load(__PACKAGE__, $VERSION);
}

sub NULL_HWADDR() { "00:00:00:00:00:00" }

sub IFF_UP() { 0x1 }

sub IFF_BROADCAST() { 0x2 }

sub IFF_DEBUG() { 0x4 }

sub IFF_LOOPBACK() { 0x8 }

sub IFF_POINTOPOINT() { 0x10 }

sub IFF_NOTRAILERS() { 0x20 }

sub IFF_RUNNING() { 0x40 }

sub IFF_NOARP() { 0x80 }

sub IFF_PROMISC() { 0x100 }

sub IFF_ALLMULTI() { 0x200 }

sub IFF_MULTICAST() { 0x800 }

sub net_interface_flags_string {
    my($flags) = @_;
    my $retval = "";

    if ($flags == 0) {
        $retval .= "[NO FLAGS] ";
    }
    if ($flags & IFF_UP) {
        $retval .= "UP ";
    }
    if ($flags & IFF_BROADCAST) {
        $retval .= "BROADCAST ";
    }
    if ($flags & IFF_DEBUG) {
        $retval .= "DEBUG ";
    }
    if ($flags & IFF_LOOPBACK) {
        $retval .= "LOOPBACK ";
    }
    if ($flags & IFF_POINTOPOINT) {
        $retval .= "POINTOPOINT ";
    }
    if ($flags & IFF_NOTRAILERS) {
        $retval .= "NOTRAILERS ";
    }
    if ($flags & IFF_RUNNING) {
        $retval .= "RUNNING ";
    }
    if ($flags & IFF_NOARP) {
        $retval .= "NOARP ";
    }
    if ($flags & IFF_PROMISC) {
        $retval .= "PROMISC ";
    }
    if ($flags & IFF_ALLMULTI) {
        $retval .= "ALLMULTI ";
    }
    if ($flags & IFF_MULTICAST) {
        $retval .= "MULTICAST ";
    }

    return $retval;
}
1;
__END__
