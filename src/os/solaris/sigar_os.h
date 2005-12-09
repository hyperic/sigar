#ifndef SIGAR_OS_H
#define SIGAR_OS_H

typedef unsigned long long int u_int64_t;

#include <ctype.h>
#include <assert.h>
#ifndef DMALLOC
#include <malloc.h>
#endif
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/processor.h>
#include <sys/sysinfo.h>
#include <sys/param.h>

#include <kstat.h>
#include <procfs.h>

#include "get_mib2.h"

/* avoid -Wall warning since solaris doesnt have a prototype for this */
int getdomainname(char *, int);

typedef struct {
    kstat_t **ks;
    int num;
    char *name;
    int nlen;
} kstat_list_t;

SIGAR_INLINE kid_t sigar_kstat_update(sigar_t *sigar);

int sigar_get_kstats(sigar_t *sigar);

int sigar_get_multi_kstats(sigar_t *sigar,
                           kstat_list_t *kl,
                           const char *name,
                           kstat_t **retval);

void sigar_koffsets_lookup(kstat_t *ksp, int *offsets, int kidx);

int sigar_proc_psinfo_get(sigar_t *sigar, sigar_pid_t pid);

int sigar_proc_usage_get(sigar_t *sigar, prusage_t *prusage, sigar_pid_t pid);

int sigar_proc_status_get(sigar_t *sigar, pstatus_t *pstatus, sigar_pid_t pid);

#define CPU_ONLINE(n) \
    (p_online(n, P_STATUS) == P_ONLINE)

/* loopback interface only has these two metrics */
typedef enum {
    KSTAT_LO_RX_PACKETS,
    KSTAT_LO_TX_PACKETS,
    KSTAT_LO_MAX
} kstat_lo_off_e;

/* hme, ge and dmfe network devices provide
 * the same metrics, but in some cases with
 * different names and in all cases, the
 * offsets are different.
 */
typedef enum {
    KSTAT_HME_RX_PACKETS,
    KSTAT_HME_RX_BYTES,
    KSTAT_HME_RX_ERRORS,
    KSTAT_HME_RX_DROPPED,
    KSTAT_HME_RX_OVERRUNS,
    KSTAT_HME_RX_FRAME,
    KSTAT_HME_TX_PACKETS,
    KSTAT_HME_TX_BYTES,
    KSTAT_HME_TX_ERRORS,
    KSTAT_HME_TX_DROPPED,
    KSTAT_HME_TX_OVERRUNS,
    KSTAT_HME_TX_COLLISIONS,
    KSTAT_HME_TX_CARRIER,
    KSTAT_HME_MAX
} kstat_hme_off_e;

typedef enum {
    KSTAT_DMFE_RX_PACKETS,
    KSTAT_DMFE_RX_BYTES,
    KSTAT_DMFE_RX_ERRORS,
    KSTAT_DMFE_RX_DROPPED,
    KSTAT_DMFE_RX_OVERRUNS,
    KSTAT_DMFE_RX_FRAME,
    KSTAT_DMFE_TX_PACKETS,
    KSTAT_DMFE_TX_BYTES,
    KSTAT_DMFE_TX_ERRORS,
    KSTAT_DMFE_TX_DROPPED,
    KSTAT_DMFE_TX_OVERRUNS,
    KSTAT_DMFE_TX_COLLISIONS,
    KSTAT_DMFE_TX_CARRIER,
    KSTAT_DMFE_MAX
} kstat_dmfe_off_e;

typedef enum {
    KSTAT_GE_RX_PACKETS,
    KSTAT_GE_RX_BYTES,
    KSTAT_GE_RX_ERRORS,
    KSTAT_GE_RX_DROPPED,
    KSTAT_GE_RX_OVERRUNS,
    KSTAT_GE_RX_FRAME,
    KSTAT_GE_TX_PACKETS,
    KSTAT_GE_TX_BYTES,
    KSTAT_GE_TX_ERRORS,
    KSTAT_GE_TX_DROPPED,
    KSTAT_GE_TX_OVERRUNS,
    KSTAT_GE_TX_COLLISIONS,
    KSTAT_GE_TX_CARRIER,
    KSTAT_GE_MAX
} kstat_ge_off_e;

typedef enum {
    KSTAT_ERI_RX_PACKETS,
    KSTAT_ERI_RX_BYTES,
    KSTAT_ERI_RX_ERRORS,
    KSTAT_ERI_RX_DROPPED,
    KSTAT_ERI_RX_OVERRUNS,
    KSTAT_ERI_RX_FRAME,
    KSTAT_ERI_TX_PACKETS,
    KSTAT_ERI_TX_BYTES,
    KSTAT_ERI_TX_ERRORS,
    KSTAT_ERI_TX_DROPPED,
    KSTAT_ERI_TX_OVERRUNS,
    KSTAT_ERI_TX_COLLISIONS,
    KSTAT_ERI_TX_CARRIER,
    KSTAT_ERI_MAX
} kstat_eri_off_e;

typedef enum {
    KSTAT_SYSTEM_BOOT_TIME,
    KSTAT_SYSTEM_LOADAVG_1,
    KSTAT_SYSTEM_LOADAVG_2,
    KSTAT_SYSTEM_LOADAVG_3,
    KSTAT_SYSTEM_MAX
} kstat_system_off_e;

typedef enum {
    KSTAT_MEMPAGES_ANON,
    KSTAT_MEMPAGES_EXEC,
    KSTAT_MEMPAGES_VNODE,
    KSTAT_MEMPAGES_MAX
} kstat_mempages_off_e;

typedef enum {
    KSTAT_SYSPAGES_FREE,
    KSTAT_SYSPAGES_MAX
} kstat_syspages_off_e;

enum {
    KSTAT_KEYS_lo,
    KSTAT_KEYS_hme,
    KSTAT_KEYS_dmfe,
    KSTAT_KEYS_ge,
    KSTAT_KEYS_eri,
    KSTAT_KEYS_system,
    KSTAT_KEYS_mempages,
    KSTAT_KEYS_syspages,
} kstat_keys_e;

typedef struct ps_prochandle * (*proc_grab_func_t)(pid_t, int, int *);

typedef void (*proc_free_func_t)(struct ps_prochandle *);

typedef void (*proc_objname_func_t)(struct ps_prochandle *,
                                    uintptr_t, const char *, size_t);

typedef char * (*proc_dirname_func_t)(const char *, char *, size_t);

typedef char * (*proc_exename_func_t)(struct ps_prochandle *, char *, size_t);

struct sigar_t {
    SIGAR_T_BASE;

    int solaris_version;

    kstat_ctl_t *kc;

    /* kstat_lookup() as needed */
    struct {
        kstat_t *vminfo;
        kstat_t **cpu;
        processorid_t *cpuid;
        unsigned int lcpu; /* number malloced slots in the cpu array above */
        kstat_t *system;
        kstat_t *syspages;
        kstat_t *mempages;
        kstat_list_t hme;
        kstat_list_t dmfe;
        kstat_list_t ge;
        kstat_list_t eri;
        kstat_list_t lo;
    } ks;

    struct {
        int lo[KSTAT_LO_MAX];
        int hme[KSTAT_HME_MAX];
        int dmfe[KSTAT_DMFE_MAX];
        int ge[KSTAT_GE_MAX];
        int eri[KSTAT_ERI_MAX];
        int system[KSTAT_SYSTEM_MAX];
        int mempages[KSTAT_MEMPAGES_MAX];
        int syspages[KSTAT_SYSPAGES_MAX];
    } koffsets;
    
    vminfo_t vminfo;
    hrtime_t vminfo_snaptime;

    int pagesize;

    time_t last_getprocs;
    sigar_pid_t last_pid;
    psinfo_t *pinfo;
    sigar_cpu_list_t cpulist;

    /* libproc.so interface */
    void *plib;
    proc_grab_func_t pgrab;
    proc_free_func_t pfree;
    proc_objname_func_t pobjname;
    proc_dirname_func_t pdirname;
    proc_exename_func_t pexename;

    sigar_cache_t *fsdev;

    solaris_mib2_t mib2;
};

#define kSTAT_uint(v, type) \
    ((sigar->koffsets.type[v] == -2) ? 0 : \
    ((kstat_named_t *)ksp->ks_data + sigar->koffsets.type[v])->value.ui32)

#define kSYSTEM(v) kSTAT_uint(v, system)

#define kMEMPAGES(v) kSTAT_uint(v, mempages)

#define kSYSPAGES(v) kSTAT_uint(v, syspages)

#define sigar_koffsets_init(sigar, ksp, type) \
    if (sigar->koffsets.type[0] == -1) \
        sigar_koffsets_lookup(ksp, sigar->koffsets.type, KSTAT_KEYS_##type)

#define sigar_koffsets_init_lo(sigar, ksp) \
    sigar_koffsets_init(sigar, ksp, lo)

#define sigar_koffsets_init_hme(sigar, ksp) \
    sigar_koffsets_init(sigar, ksp, hme)

#define sigar_koffsets_init_dmfe(sigar, ksp) \
    sigar_koffsets_init(sigar, ksp, dmfe)

#define sigar_koffsets_init_ge(sigar, ksp) \
    sigar_koffsets_init(sigar, ksp, ge)

#define sigar_koffsets_init_eri(sigar, ksp) \
    sigar_koffsets_init(sigar, ksp, eri)

#define sigar_koffsets_init_system(sigar, ksp) \
    sigar_koffsets_init(sigar, ksp, system)

#define sigar_koffsets_init_mempages(sigar, ksp) \
    sigar_koffsets_init(sigar, ksp, mempages)

#define sigar_koffsets_init_syspages(sigar, ksp) \
    sigar_koffsets_init(sigar, ksp, syspages)

#define HAVE_READDIR_R
#define HAVE_GETPWNAM_R
#define HAVE_GETPWUID_R

#define SIGAR_EMIB2 (SIGAR_OS_START_ERROR+1)

#endif /* SIGAR_OS_H */

