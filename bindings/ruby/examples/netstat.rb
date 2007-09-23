require 'rbsigar'
require 'socket'

#XXX this example is incomplete wrt:
#../../java/src/org/hyperic/sigar/cmd/Netstat.java

is_numeric = false
flags = Sigar::NETCONN_CLIENT|Sigar::NETCONN_SERVER
flags |= Sigar::NETCONN_TCP

def format_port(sigar, proto, port, is_numeric)
  if port == 0
    return "*"
  end
  if !is_numeric
    service = sigar.net_services_name(proto, port)
    if service != nil
      return service
    end
  end
  port.to_s
end

def format_address(sigar, proto, ip, portnum, is_numeric)
  port = format_port(sigar, proto, portnum, is_numeric)
  address = ""
  if ip == "0.0.0.0" || ip == "::"
    address = "*"
  elsif is_numeric
    address = ip.to_s
  else
    begin
      name = Socket.gethostbyname(ip)
      address = name[0]
    rescue SocketError
      address = ip.to_s
    end
  end
  return address + ":" + port
end

sigar = Sigar.new

connections = sigar.net_connection_list(flags)
puts "Proto\tLocal Address\tForeign Address\tState"

connections.each do |conn|
  proto = Sigar.net_connection_type_to_s(conn.type)
  state = Sigar.net_connection_state_to_s(conn.state)
  local = format_address(sigar, conn.type, conn.local_address, conn.local_port, is_numeric)
  remote = format_address(sigar, conn.type, conn.remote_address, conn.remote_port, is_numeric)
  puts proto + "\t" + local + "\t" + remote + "\t" + state
end

