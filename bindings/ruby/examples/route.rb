#
# Copyright (c) 2009 SpringSource, Inc.
# Copyright (c) 2010 VMware, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'sigar'

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
