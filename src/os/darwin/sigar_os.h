#ifndef SIGAR_OS_H
#define SIGAR_OS_H

#ifdef DARWIN
#include <mach/port.h>
#include <mach/host_info.h>
#else
#include <kvm.h>
#endif

#include <sys/sysctl.h>

struct sigar_t {
    SIGAR_T_BASE;
    int pagesize;
    time_t last_getprocs;
    sigar_pid_t last_pid;
    struct kinfo_proc *pinfo;
#ifdef DARWIN
    mach_port_t mach_port;
#else
    kvm_t *kp;
#endif
};

#endif /* SIGAR_OS_H */
