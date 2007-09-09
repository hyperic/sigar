import os, sigar;

sg = sigar.open()
fslist = sg.file_system_list()

def format_size(size):
    return sigar.format_size(size * 1024)

print 'Filesystem\tSize\tUsed\tAvail\tUse%\tMounted on\tType\n'

for fs in fslist:
    dir_name = fs.dir_name()
    usage = sg.file_system_usage(dir_name)

    total = usage.total()
    used = total - usage.free()
    avail = usage.avail()
    pct = usage.use_percent() * 100
    if pct == 0.0:
        pct = '-'

    print fs.dev_name(), format_size(total), format_size(used), format_size(avail),\
        pct, dir_name, fs.sys_type_name(), '/', fs.type_name()
