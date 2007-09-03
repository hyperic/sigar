require 'rbsigar'

def format_size(size)
  return Sigar.format_size(size * 1024)
end

sigar = Sigar.new
fslist = sigar.file_system_list

puts "Filesystem\tSize\tUsed\tAvail\tUse%\tMounted on\tType\n"

fslist.each do |fs|
  dir_name = fs.dir_name
  usage = sigar.file_system_usage(dir_name)

  total = usage.total
  used = total - usage.free
  avail = usage.avail
  pct = usage.use_percent * 100

  puts fs.dev_name + "\t" +
    format_size(total) + "\t" +
    format_size(used) + "\t" +
    format_size(avail) + "\t" +
    (pct == 0.0 ? '-' : pct.to_s) + "\t" +
    dir_name + "\t" + 
    fs.sys_type_name + "/" + fs.type_name
end
