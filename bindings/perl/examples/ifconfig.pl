#!/usr/bin/perl

use strict;
use Sigar;

my $sigar = new Sigar;

my $iflist = $sigar->net_interface_list;

for my $ifname (@$iflist) {
    my $ifconfig = $sigar->net_interface_config($ifname);
    my $flags = $ifconfig->flags;

    my $encap = $ifconfig->type;

    my $hwaddr = $ifconfig->hwaddr;
    if ($hwaddr eq Sigar::NULL_HWADDR) {
        $hwaddr = "";
    }
    else {
        $hwaddr = " HWaddr " . $hwaddr;
    }

    print $ifname . "\t" . "Link encap:" . $encap . $hwaddr . "\n";

    my $ptp = "";
    if ($flags & Sigar::IFF_POINTOPOINT) {
        $ptp = "  P-t-P:" . $ifconfig->destination;
    }

    my $bcast = "";
    if ($flags & Sigar::IFF_BROADCAST) {
        $bcast = "  Bcast:" . $ifconfig->broadcast;
    }

    print "\t" . "inet addr:" . $ifconfig->address .
      $ptp . $bcast . "  Mask:" . $ifconfig->netmask . "\n";

    print "\t" .
      Sigar::net_interface_flags_string($flags) .
      " MTU:" . $ifconfig->mtu .
      "  Metric:" . $ifconfig->metric . "\n";

    my $ifstat;
    eval {
        $ifstat = $sigar->net_interface_stat($ifname);
    } or next;

    print "\t" .
      "RX packets:" . $ifstat->rx_packets .
      " errors:" . $ifstat->rx_errors .
      " dropped:" . $ifstat->rx_dropped .
      " overruns:" . $ifstat->rx_overruns .
      " frame:" . $ifstat->rx_frame . "\n";

    print "\t" .
      "TX packets:" . $ifstat->tx_packets .
      " errors:" . $ifstat->tx_errors .
      " dropped:" . $ifstat->tx_dropped .
      " overruns:" . $ifstat->tx_overruns .
      " carrier:" . $ifstat->tx_carrier . "\n";

    print "\t" . "collisions:" . $ifstat->tx_collisions . "\n";

    my $rx_bytes = $ifstat->rx_bytes;
    my $tx_bytes = $ifstat->tx_bytes;

    print "\t" .
      "RX bytes:" . $rx_bytes .
        " (" . Sigar::format_size($rx_bytes) . ")" .
        "  " .
        "TX bytes:" . $tx_bytes .
        " (" . Sigar::format_size($tx_bytes) . ")" . "\n\n";
}
