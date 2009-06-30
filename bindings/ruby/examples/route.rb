require 'rbsigar'

def flags(flags)
  f = ""
  if (flags & Sigar::RTF_UP) != 0
    f += "U"
  end
  if (flags & Sigar::RTF_GATEWAY) != 0
    f += "G"
  end
  if (flags & Sigar::RTF_HOST) != 0
    f += "H"
  end
  f
end

def gw(addr)
  addr == "0.0.0.0" ? "*" : addr
end
def dest(addr)
  addr == "0.0.0.0" ? "default" : addr
end

puts "Kernel IP routing table"
fmt = "%-15s %-15s %-15s %-5s %-6s %-3s %-s\n"
printf fmt, "Destination", "Gateway", "Genmask", "Flags", "Metric", "Ref", "Iface"

Sigar.new.net_route_list.each do |route|
  printf fmt, dest(route.destination), gw(route.gateway), route.mask,
    flags(route.flags), route.metric.to_s, route.refcnt.to_s, route.ifname
end
