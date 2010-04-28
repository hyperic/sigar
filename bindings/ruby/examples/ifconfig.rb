#
# Copyright (c) 2007 Hyperic, Inc.
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

sigar = Sigar.new
iflist = sigar.net_interface_list

iflist.each do |ifname|
  ifconfig = sigar.net_interface_config(ifname)
  flags = ifconfig.flags
  encap = ifconfig.type

  if (flags & Sigar::IFF_UP) == 0
    next
  end

  hwaddr = ifconfig.hwaddr
  if hwaddr == Sigar::NULL_HWADDR
    hwaddr = ""
  else
    hwaddr = " HWaddr " + hwaddr
  end

  puts ifname + "\t" + "Link encap:" + encap + hwaddr

  if (flags & Sigar::IFF_POINTOPOINT) != 0
    ptp = "  P-t-P:" + ifconfig.destination
  else
    ptp = ""
  end

  if (flags & Sigar::IFF_BROADCAST) != 0
    bcast = "  Bcast:" + ifconfig.broadcast
  else
    bcast = ""
  end

  address = ifconfig.address
  if address != "0.0.0.0"
    puts "\t" + "inet addr:" + address +
      ptp + bcast + " Mask:" + ifconfig.netmask
  end

  if ifconfig.prefix6_length != 0
    puts "\t" + "inet6 addr: " + ifconfig.address6 + "/" +
      ifconfig.prefix6_length.to_s + " Scope:" + Sigar.net_scope_to_s(ifconfig.scope6)
  end

  puts "\t" +
    Sigar.net_interface_flags_to_s(flags) +
    " MTU:" + ifconfig.mtu.to_s +
    " Metric:" + ifconfig.metric.to_s

  if (!ifname.include?(":"))
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

    puts "\t" + "collisions:" + ifstat.tx_collisions.to_s +
      " txqueuelen:" + ifconfig.tx_queue_len.to_s

    rx_bytes = ifstat.rx_bytes
    tx_bytes = ifstat.tx_bytes

    puts "\t" +
      "RX bytes:" + rx_bytes.to_s +
      " (" + Sigar.format_size(rx_bytes) + ")" +
      "  " +
      "TX bytes:" + tx_bytes.to_s +
      " (" + Sigar.format_size(tx_bytes) + ")" + "\n"
  end

  puts "\n"
end
