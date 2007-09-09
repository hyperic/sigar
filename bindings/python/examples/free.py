import os, sigar;

sg = sigar.open()
mem = sg.mem()
swap = sg.swap()
sg.close()

print "\tTotal\tUsed\tFree"

print "Mem:\t",\
    (mem.total() / 1024), \
    (mem.used() / 1024), \
    (mem.free() / 1024)

print "Swap:\t", \
    (swap.total() / 1024), \
    (swap.used() / 1024), \
    (swap.free() / 1024)

print "RAM:\t", mem.ram(), "MB"

