#ifndef SIGAR_OS_H
#define SIGAR_OS_H

#include <fcntl.h>
#include <errno.h>
#include <dlfcn.h>
#include <procinfo.h>
#include <sys/resource.h>

#include "libperfstat.h"

enum {
    KOFFSET_LOADAVG,
    KOFFSET_VAR,
    KOFFSET_SYSINFO,
    KOFFSET_IFNET,
    KOFFSET_VMINFO,
    KOFFSET_CPUINFO,
    KOFFSET_TCB,
    KOFFSET_MAX
};

typedef struct {
    time_t mtime;
    int num;
    char **devs;
} swaps_t;

typedef int (*vminfo_func_t) (void *, int, int);

typedef int (*proc_fd_func_t) (sigar_t *, sigar_pid_t, sigar_proc_fd_t *);

typedef int (*perfstat_cpu_total_func_t)(perfstat_cpu_total_t *, size_t);

typedef int (*perfstat_cpu_func_t)(perfstat_id_t *,
                                   perfstat_cpu_t *,
                                   size_t, int);

typedef int (*perfstat_swap_func_t)(perfstat_id_t *,
                                    perfstat_pagingspace_t *,
                                    size_t, int);

typedef int (*perfstat_disk_func_t)(perfstat_id_t *,
                                    perfstat_disk_t *,
                                    size_t, int);

typedef int (*perfstat_ifstat_func_t)(perfstat_id_t *,
                                      perfstat_netinterface_t *);

typedef int (*thread_rusage_func_t)(struct rusage *, int);

struct sigar_t {
    SIGAR_T_BASE;
    int kmem;
    int dmem;
    /* offsets for seeking on kmem */
    long koffsets[KOFFSET_MAX];
    vminfo_func_t getvminfo;
    proc_fd_func_t getprocfd;
    struct {
        int avail;
        perfstat_cpu_func_t cpu;
        perfstat_cpu_total_func_t cpu_total;
        perfstat_swap_func_t swap;
        perfstat_disk_func_t disk;
        perfstat_ifstat_func_t ifstat;
        thread_rusage_func_t thread_rusage;
        void *handle;
    } perfstat;
    int pagesize;
    swaps_t swaps;
    time_t last_getprocs;
    sigar_pid_t last_pid;
    struct procsinfo64 *pinfo;
    struct cpuinfo *cpuinfo;
    int cpuinfo_size;
    int cpu_mhz;
    char model[128];
    int aix_version;
    int thrusage;
    sigar_cache_t *diskmap; 
};

#define HAVE_STRERROR_R

#define SIGAR_EPERM_KMEM (SIGAR_OS_START_ERROR+1)

#endif /* SIGAR_OS_H */
