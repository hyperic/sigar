require 'sigar'

Sigar.new.arp_list.each do |arp|
  host = "?" #XXX
  puts host + " " +
    "(" + arp.address + ")" + " at " + arp.hwaddr + " " +
    "[" + arp.type + "]" + " on " + arp.ifname
end
