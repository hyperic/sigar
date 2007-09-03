require 'rbsigar'

sigar = Sigar.new
mem = sigar.mem
swap = sigar.swap

puts "\tTotal\tUsed\tFree"

puts "Mem:    " +
     (mem.total / 1024).to_s + "\t" +
     (mem.used / 1024).to_s + "\t" +
     (mem.free/ 1024).to_s

puts "Swap:   " +
     (swap.total / 1024).to_s + "\t" +
     (swap.used / 1024).to_s + "\t" +
     (swap.free/ 1024).to_s

puts "RAM:    " + mem.ram.to_s + "MB";
