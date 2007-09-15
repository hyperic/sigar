require 'rbsigar'

sigar = Sigar.new

infos = sigar.cpu_info_list

num = infos.length

puts num.to_s + " total CPUs.."

infos.each do |info|
    puts "Vendor........" + info.vendor
    puts "Model........." + info.model
    puts "Mhz..........." + info.mhz.to_s
    puts "Cache size...." + info.cache_size.to_s
end
