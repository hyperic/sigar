require 'rbsigar'

sigar = Sigar.new
iflist = sigar.net_interface_list

iflist.each do |ifname|
  ifconfig = sigar.net_interface_config(ifname)
  flags = ifconfig.flags
  encap = ifconfig.type
  hwaddr = ifconfig.hwaddr

  puts ifname + "\t" + "Link encap:" + encap + hwaddr
  ptp = "" #XXX
  bcast = "" #XXX

  puts "\t" + "inet addr:" + ifconfig.address +
    ptp + bcast + " Mask:" + ifconfig.netmask

  puts "\t" +
    flags.to_s + #XXX
    " MTU:" + ifconfig.mtu.to_s +
    " Metric:" + ifconfig.metric.to_s

  ifstat = sigar.net_interface_stat(ifname)

  puts "\t" +
      "RX packets:" + ifstat.rx_packets.to_s +
      " errors:" + ifstat.rx_errors.to_s +
      " dropped:" + ifstat.rx_dropped.to_s +
      " overruns:" + ifstat.rx_overruns.to_s +
      " frame:" + ifstat.rx_frame.to_s

  puts "\t" +
      "TX packets:" + ifstat.tx_packets.to_s +
      " errors:" + ifstat.tx_errors.to_s +
      " dropped:" + ifstat.tx_dropped.to_s +
      " overruns:" + ifstat.tx_overruns.to_s +
      " carrier:" + ifstat.tx_carrier.to_s

  puts "\t" + "collisions:" + ifstat.tx_collisions.to_s

  rx_bytes = ifstat.rx_bytes
  tx_bytes = ifstat.tx_bytes

  print "\t" +
    "RX bytes:" + rx_bytes.to_s +
    " (" + Sigar.format_size(rx_bytes) + ")" +
    "  " +
    "TX bytes:" + tx_bytes.to_s +
    " (" + Sigar.format_size(tx_bytes) + ")" + "\n";
end
