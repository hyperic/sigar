#
# Copyright (c) 2009-2010 VMware, Inc.
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

# Example illustrating the collecting of network information.

require 'sigar'

sigar = Sigar.new

ninfo = sigar.net_info

puts "Hostname:                  " + ninfo.host_name
puts "Domain Name:               " + ninfo.domain_name
puts "FQDN:                      " + sigar.fqdn
puts "Default gateway:           " + ninfo.default_gateway
puts "Default gateway interface: " + ninfo.default_gateway_interface
puts "Primary DNS:               " + ninfo.primary_dns
puts "Secondary DNS:             " + ninfo.secondary_dns
