#ifndef SIGAR_OS_H
#define SIGAR_OS_H

#include <sys/vm.h>

#include <mach.h>
#include <mach/mach_types.h>
#include <mach/vm_statistics.h>

struct sigar_t {
    SIGAR_T_BASE;
};

#endif /* SIGAR_OS_H */
