#ifndef SIGAR_OS_H
#define SIGAR_OS_H

#include <sys/table.h>
/* "i will *punch* you in the *face*" --will ferrell */
#undef idle
#undef usr
#undef sys

#include <errno.h>
#include <sys/vm.h>

#include <mach.h>
#include <mach/mach_types.h>
#include <mach/vm_statistics.h>

struct sigar_t {
    SIGAR_T_BASE;
    int pagesize;
};

#endif /* SIGAR_OS_H */
