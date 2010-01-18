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
