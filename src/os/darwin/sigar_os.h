#ifndef SIGAR_OS_H
#define SIGAR_OS_H

#ifdef DARWIN
#include <mach/port.h>
#include <mach/host_info.h>
#else
#include <kvm.h>
#endif

#include <sys/sysctl.h>

enum {
    KOFFSET_CPUINFO,
    KOFFSET_VMMETER,
    KOFFSET_MAX
};

struct sigar_t {
    SIGAR_T_BASE;
    int pagesize;
    time_t last_getprocs;
    sigar_pid_t last_pid;
    struct kinfo_proc *pinfo;
#ifdef DARWIN
    mach_port_t mach_port;
#else
    kvm_t *kmem;
    /* offsets for seeking on kmem */
    unsigned long koffsets[KOFFSET_MAX];
    int proc_mounted;
#endif
};

#define SIGAR_EPERM_KMEM (SIGAR_OS_START_ERROR+EACCES)
#define SIGAR_EPROC_NOENT (SIGAR_OS_START_ERROR+2)

#endif /* SIGAR_OS_H */
