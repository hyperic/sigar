#ifndef SIGAR_OS_H
#define SIGAR_OS_H

#include <fcntl.h>
#include <errno.h>
#include <dlfcn.h>
#include <procinfo.h>

enum {
    KOFFSET_LOADAVG,
    KOFFSET_VAR,
    KOFFSET_SYSINFO,
    KOFFSET_IFNET,
    KOFFSET_VMINFO,
    KOFFSET_CPUINFO,
    KOFFSET_MAX
};

typedef struct {
    time_t mtime;
    int num;
    char **devs;
} swaps_t;

typedef int (*vminfo_func_t) (void *, int, int);

typedef int (*proc_fd_func_t) (sigar_t *, sigar_pid_t, sigar_proc_fd_t *);

struct sigar_t {
    SIGAR_T_BASE;
    int kmem;
    /* offsets for seeking on kmem */
    long koffsets[KOFFSET_MAX];
    vminfo_func_t getvminfo;
    proc_fd_func_t getprocfd;
    int pagesize;
    swaps_t swaps;
    time_t last_getprocs;
    sigar_pid_t last_pid;
    struct procsinfo *pinfo;
    struct cpuinfo *cpuinfo;
    int cpuinfo_size;
    int cpu_mhz;
    char model[128];
    char self_path[SIGAR_PATH_MAX]; /* path to where libsigar.so lives */
};

#define HAVE_STRERROR_R

#define SIGAR_EPERM_KMEM (SIGAR_OS_START_ERROR+1)

#endif /* SIGAR_OS_H */
