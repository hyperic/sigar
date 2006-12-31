/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

#include <inet/ip.h>
#include <inet/tcp.h>
#include <net/route.h>
#include <sys/link.h>
#include <sys/lwp.h>
#include <sys/proc.h>
#include <sys/swap.h>
#include <sys/stat.h>
#include <sys/systeminfo.h>
#include <sys/utsname.h>
#include <dlfcn.h>

#define KSTAT_LIST_INIT(sigar, dev) \
    sigar->koffsets.dev[0] = -1; \
    sigar->ks.dev.num = 0; \
    sigar->ks.dev.ks  = NULL; \
    sigar->ks.dev.name = #dev; \
    sigar->ks.dev.nlen = strlen(#dev)

#define PROC_ERRNO ((errno == ENOENT) ? ESRCH : errno)

int sigar_os_open(sigar_t **sig)
{
    kstat_ctl_t *kc;
    kstat_t *ksp;
    sigar_t *sigar;
    int i, status;
    struct utsname name;
    char *ptr;

    sigar = malloc(sizeof(*sigar));
    *sig = sigar;

    sigar->log_level = -1; /* log nothing by default */
    sigar->log_impl = NULL;
    sigar->log_data = NULL;

    uname(&name);
    if ((ptr = strchr(name.release, '.'))) {
        ptr++;
        sigar->solaris_version = atoi(ptr);
    }
    else {
        sigar->solaris_version = 6;
    }

    if ((ptr = getenv("SIGAR_USE_UCB_PS"))) {
        sigar->use_ucb_ps = strEQ(ptr, "true");
    }
    else {
        sigar->use_ucb_ps = 1;
    }

    sigar->pagesize = 0;
    i = sysconf(_SC_PAGESIZE);
    while ((i >>= 1) > 0) {
        sigar->pagesize++;
    }

    sigar->ticks = sysconf(_SC_CLK_TCK);
    sigar->kc = kc = kstat_open();

    if (!kc) {
        return errno;
    }

    sigar->cpulist.size = 0;
    sigar->ncpu = 0;
    sigar->ks.cpu = NULL;
    sigar->ks.cpu_info = NULL;
    sigar->ks.cpuid = NULL;
    sigar->ks.lcpu = 0;

    KSTAT_LIST_INIT(sigar, lo);
    KSTAT_LIST_INIT(sigar, hme);
    KSTAT_LIST_INIT(sigar, dmfe);
    KSTAT_LIST_INIT(sigar, ge);
    KSTAT_LIST_INIT(sigar, eri);

    sigar->koffsets.system[0] = -1;
    sigar->koffsets.mempages[0] = -1;
    sigar->koffsets.syspages[0] = -1;
    
    if ((status = sigar_get_kstats(sigar)) != SIGAR_OK) {
        fprintf(stderr, "status=%d\n", status);
    } 

    sigar->boot_time = 0;

    if ((ksp = sigar->ks.system) &&
        (kstat_read(kc, ksp, NULL) >= 0))
    {
        sigar_koffsets_init_system(sigar, ksp);

        sigar->boot_time = kSYSTEM(KSTAT_SYSTEM_BOOT_TIME);
    }

    sigar->last_pid = -1;
    sigar->pinfo = NULL;

    sigar->plib = NULL;
    sigar->pgrab = NULL;
    sigar->pfree = NULL;
    sigar->pobjname = NULL;

    sigar->fsdev = NULL;
    sigar->pargs = NULL;

    SIGAR_ZERO(&sigar->mib2);
    sigar->mib2.sd = -1;

    return SIGAR_OK;
}

int sigar_os_close(sigar_t *sigar)
{
    kstat_close(sigar->kc);

    if (sigar->ks.lo.num) {
        free(sigar->ks.lo.ks);
    }
    if (sigar->ks.hme.num) {
        free(sigar->ks.hme.ks);
    }
    if (sigar->ks.dmfe.num) {
        free(sigar->ks.dmfe.ks);
    }
    if (sigar->ks.ge.num) {
        free(sigar->ks.ge.ks);
    }
    if (sigar->ks.eri.num) {
        free(sigar->ks.eri.ks);
    }
    if (sigar->ks.lcpu) {
        free(sigar->ks.cpu);
        free(sigar->ks.cpu_info);
        free(sigar->ks.cpuid);
    }
    if (sigar->pinfo) {
        free(sigar->pinfo);
    }
    if (sigar->cpulist.size != 0) {
        sigar_cpu_list_destroy(sigar, &sigar->cpulist);
    }
    if (sigar->plib) {
        dlclose(sigar->plib);
    }
    if (sigar->fsdev) {
        sigar_cache_destroy(sigar->fsdev);
    }
    if (sigar->pargs) {
        sigar_cache_destroy(sigar->pargs);
    }
    free(sigar);
    return SIGAR_OK;
}

char *sigar_os_error_string(sigar_t *sigar, int err)
{
    switch (err) {
      case SIGAR_EMIB2:
        return sigar->mib2.errmsg;
      default:
        return NULL;
    }
}

int sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
    kstat_ctl_t *kc = sigar->kc; 
    kstat_t *ksp;

    SIGAR_ZERO(mem);

    /* XXX: is mem hot swappable or can we just do this during open ? */
    mem->total = sysconf(_SC_PHYS_PAGES);
    mem->total <<= sigar->pagesize;

    sigar_mem_calc_ram(sigar, mem);

    if (sigar_kstat_update(sigar) == -1) {
        return errno;
    }

    if ((ksp = sigar->ks.syspages) && kstat_read(kc, ksp, NULL) >= 0) {
        sigar_koffsets_init_syspages(sigar, ksp);

        mem->free = kSYSPAGES(KSTAT_SYSPAGES_FREE);
        mem->free <<= sigar->pagesize;

        mem->used = mem->total - mem->free;
    }

    if ((ksp = sigar->ks.mempages) && kstat_read(kc, ksp, NULL) >= 0) {
        sigar_koffsets_init_mempages(sigar, ksp);
    }

    mem->actual_free = mem->free;
    mem->actual_used = mem->used;

    return SIGAR_OK;
}

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
    struct anoninfo anon;

    /* XXX vm/anon.h says:
     * "The swap data can be aquired more efficiently through the
     *  kstats interface."
     * but cannot find anything that explains howto convert those numbers.
     */

    if (swapctl(SC_AINFO, &anon) == -1) {
        return errno;
    }

    swap->total = anon.ani_max;
    swap->used  = anon.ani_resv;
    swap->free  = anon.ani_max - anon.ani_resv;

    swap->total <<= sigar->pagesize;
    swap->free  <<= sigar->pagesize;
    swap->used  <<= sigar->pagesize;

    return SIGAR_OK;
}

#ifndef KSTAT_NAMED_STR_PTR
/* same offset as KSTAT_NAMED_STR_PTR(brand) */
#define KSTAT_NAMED_STR_PTR(n) (char *)((n)->value.i32)
#endif

static int get_chip_brand(sigar_t *sigar, int processor,
                          sigar_cpu_info_t *info)
{
    kstat_t *ksp = sigar->ks.cpu_info[processor];
    kstat_named_t *brand;

    if (sigar->solaris_version < 10) {
        /* don't bother; doesn't exist. */
        return 0;
    }

    if (ksp &&
        (kstat_read(sigar->kc, ksp, NULL) != -1) &&
        (brand = (kstat_named_t *)kstat_data_lookup(ksp, "brand")))
    {
        char *name = KSTAT_NAMED_STR_PTR(brand);

        char *vendor = "Sun";
        char *vendors[] = {
            "Intel", "AMD", NULL
        };
        int i;

        if (!name) {
            return 0;
        }

        for (i=0; vendors[i]; i++) {
            if (strstr(name, vendors[i])) {
                vendor = vendors[i];
                break;
            }
        }

        SIGAR_SSTRCPY(info->vendor, vendor);
#if 0
        SIGAR_SSTRCPY(info->model, name);
        sigar_cpu_model_adjust(sigar, info);
#endif
        return 1;
    }
    else {
        return 0;
    }
}

static int get_chip_id(sigar_t *sigar, int processor)
{
    kstat_t *ksp = sigar->ks.cpu_info[processor];
    kstat_named_t *chipid;

    if (ksp &&
        (kstat_read(sigar->kc, ksp, NULL) != -1) &&
        (chipid = (kstat_named_t *)kstat_data_lookup(ksp, "chip_id")))
    {
        return chipid->value.i32;
    }
    else {
        return -1;
    }
}

static int is_same_chip(sigar_t *sigar, int processor, int num)
{
    int chip_id = get_chip_id(sigar, processor);

    if ((chip_id == -1) || (chip_id != (num-1))) {
        return 0;
    }
    else {
        return 1;
    }
}

int sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
    int status, i;

    status = sigar_cpu_list_get(sigar, &sigar->cpulist);

    if (status != SIGAR_OK) {
        return status;
    }

    SIGAR_ZERO(cpu);

    for (i=0; i<sigar->cpulist.number; i++) {
        sigar_cpu_t *xcpu = &sigar->cpulist.data[i];

        cpu->user  += xcpu->user;
        cpu->sys   += xcpu->sys;
        cpu->idle  += xcpu->idle;
        cpu->nice  += xcpu->nice;
        cpu->wait  += xcpu->wait;
        cpu->total = xcpu->total;
    }

    return SIGAR_OK;
}

int sigar_cpu_list_get(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    kstat_ctl_t *kc = sigar->kc; 
    kstat_t *ksp;
    ulong cpuinfo[CPU_STATES];
    unsigned int i;
    int is_debug = SIGAR_LOG_IS_DEBUG(sigar);
    int reported_virtual = 0;

    if (sigar_kstat_update(sigar) == -1) {
        return errno;
    }

    if (cpulist == &sigar->cpulist) {
        if (sigar->cpulist.size == 0) {
            /* create once */
            sigar_cpu_list_create(cpulist);
        }
        else {
            /* reset, re-using cpulist.data */
            sigar->cpulist.number = 0;
        }
    }
    else {
        sigar_cpu_list_create(cpulist);
    }

    if (is_debug) {
        sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                         "[cpu_list] OS reports %d CPUs",
                         sigar->ncpu);
    }
                         
    for (i=0; i<sigar->ncpu; i++) {
        sigar_cpu_t *cpu;
        char *buf;

        if (!CPU_ONLINE(sigar->ks.cpuid[i])) {
            sigar_log_printf(sigar, SIGAR_LOG_INFO,
                             "cpu %d (id=%d) is offline",
                             i, sigar->ks.cpuid[i]);
            continue;
        }

        if (!(ksp = sigar->ks.cpu[i])) {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                             "NULL ksp for cpu %d (id=%d)",
                             i, sigar->ks.cpuid[i]);
            continue; /* shouldnot happen */
        }

        if (kstat_read(kc, ksp, NULL) < 0) {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                             "kstat_read failed for cpu %d (id=%d): %s",
                             i, sigar->ks.cpuid[i],
                             sigar_strerror(sigar, errno));
            continue; /* shouldnot happen */
        }

        /*
         * cpu_stat_t is not binary compatible between solaris versions.
         * since cpu_stat is a 'raw' kstat and not 'named' we cannot
         * use name based lookups as we do for others.
         * the start of the cpu_stat_t structure is binary compatible,
         * which looks like so:
         * typedef struct cpu_stat {
         *    kmutex_t        cpu_stat_lock;
         *    cpu_sysinfo_t   cpu_sysinfo;
         *    ...
         *    typedef struct cpu_sysinfo {
         *       ulong cpu[CPU_STATES];
         *       ...
         * we just copy the piece we need below:
         */
        buf = ksp->ks_data;
        buf += sizeof(kmutex_t);
        memcpy(&cpuinfo[0], buf, sizeof(cpuinfo));

        if (is_same_chip(sigar, i, cpulist->number)) {
            /* merge times of logical processors */
            cpu = &cpulist->data[cpulist->number-1];

            if (is_debug && !reported_virtual++) {
                sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                                 "[cpu_list] Merging times of"
                                 " logical processors");
            }
        }
        else {
            SIGAR_CPU_LIST_GROW(cpulist);
            cpu = &cpulist->data[cpulist->number++];
            SIGAR_ZERO(cpu);
        }

        cpu->user += SIGAR_TICK2MSEC(cpuinfo[CPU_USER]);
        cpu->sys  += SIGAR_TICK2MSEC(cpuinfo[CPU_KERNEL]);
        cpu->idle += SIGAR_TICK2MSEC(cpuinfo[CPU_IDLE]);
        cpu->wait += SIGAR_TICK2MSEC(cpuinfo[CPU_WAIT]);
        cpu->nice += 0; /* no cpu->nice */
        cpu->total = cpu->user + cpu->sys + cpu->idle + cpu->wait;
    }
    
    return SIGAR_OK;
}

int sigar_uptime_get(sigar_t *sigar,
                     sigar_uptime_t *uptime)
{
    if (sigar->boot_time) {
        uptime->uptime = time(NULL) - sigar->boot_time;
    }
    else {
        uptime->uptime = 0; /* XXX: shouldn't happen */
    }

    return SIGAR_OK;
}

static int loadavg_keys[] = {
    KSTAT_SYSTEM_LOADAVG_1,
    KSTAT_SYSTEM_LOADAVG_2,
    KSTAT_SYSTEM_LOADAVG_3
};

int sigar_loadavg_get(sigar_t *sigar,
                      sigar_loadavg_t *loadavg)
{
    kstat_t *ksp;
    int i;

    if (sigar_kstat_update(sigar) == -1) {
        return errno;
    }

    if (!(ksp = sigar->ks.system)) {
        return -1;
    }

    if (kstat_read(sigar->kc, ksp, NULL) < 0) {
        return -1;
    }

    sigar_koffsets_init_system(sigar, ksp);
    
    for (i=0; i<3; i++) {
        loadavg->loadavg[i] = (double)kSYSTEM(loadavg_keys[i]) / FSCALE;
    }

    return SIGAR_OK;
}

#define LIBPROC "/usr/lib/libproc.so"

#define CHECK_PSYM(s) \
    if (!sigar->s) { \
        sigar_log_printf(sigar, SIGAR_LOG_WARN, \
                         "[%s] Symbol not found: %s", \
                         SIGAR_FUNC, #s); \
        dlclose(sigar->plib); \
        sigar->plib = NULL; \
        return SIGAR_ENOTIMPL; \
    }

static char *proc_readlink(const char *name, char *buffer, size_t size)
{
    int len;

    if ((len = readlink(name, buffer, size-1)) < 0) {
        return NULL;
    }

    buffer[len] = '\0';
    return buffer;
}

static int sigar_init_libproc(sigar_t *sigar)
{
    if (sigar->plib) {
        return SIGAR_OK;
    }

    /* libproc.so ships with 5.8+ */
    /* interface is undocumented, see libproc.h in the sun jdk sources */
    sigar->plib = dlopen(LIBPROC, RTLD_LAZY);

    if (!sigar->plib) {
        sigar_log_printf(sigar, SIGAR_LOG_WARN,
                         "[%s] dlopen(%s) = %s",
                         SIGAR_FUNC, LIBPROC, dlerror());
        return SIGAR_ENOTIMPL;
    }

    sigar->pgrab    = (proc_grab_func_t)dlsym(sigar->plib, "Pgrab");
    sigar->pfree    = (proc_free_func_t)dlsym(sigar->plib, "Pfree");
    sigar->pobjname = (proc_objname_func_t)dlsym(sigar->plib, "Pobjname");
    sigar->pexename = (proc_exename_func_t)dlsym(sigar->plib, "Pexecname");
    sigar->pdirname = (proc_dirname_func_t)dlsym(sigar->plib, "proc_dirname");

    CHECK_PSYM(pgrab);
    CHECK_PSYM(pfree);
    CHECK_PSYM(pobjname);

    return SIGAR_OK;
}

/* from libproc.h, not included w/ solaris distro */
/* Error codes from Pgrab(), Pfgrab_core(), and Pgrab_core() */
#define	G_STRANGE	-1	/* Unanticipated error, errno is meaningful */
#define	G_NOPROC	1	/* No such process */
#define	G_NOCORE	2	/* No such core file */
#define	G_NOPROCORCORE	3	/* No such proc or core (for proc_arg_grab) */
#define	G_NOEXEC	4	/* Cannot locate executable file */
#define	G_ZOMB		5	/* Zombie process */
#define	G_PERM		6	/* No permission */
#define	G_BUSY		7	/* Another process has control */
#define	G_SYS		8	/* System process */
#define	G_SELF		9	/* Process is self */
#define	G_INTR		10	/* Interrupt received while grabbing */
#define	G_LP64		11	/* Process is _LP64, self is ILP32 */
#define	G_FORMAT	12	/* File is not an ELF format core file */
#define	G_ELF		13	/* Libelf error, elf_errno() is meaningful */
#define	G_NOTE		14	/* Required PT_NOTE Phdr not present in core */

static int sigar_pgrab(sigar_t *sigar, sigar_pid_t pid,
                       const char *func,
                       struct ps_prochandle **phandle)
{
    int pstatus;

    if (!(*phandle = sigar->pgrab(pid, 0x01, &pstatus))) {
        switch (pstatus) {
          case G_NOPROC:
            return ESRCH;
          case G_PERM:
            return EACCES;
          default:
            sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                             "[%s] Pgrab error=%d",
                             func, pstatus);
            return ENOTSUP; /*XXX*/
        }
    }

    return SIGAR_OK;
}

static int sigar_dlinfo_get(sigar_t *sigar, const char *func,
                            void **handle, Link_map **map)
{
    Dl_info dli;

    if (!dladdr((void *)((uintptr_t)sigar_dlinfo_get), &dli)) {
        sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                         "[%s] dladdr(%s) = %s",
                         func, SIGAR_FUNC, dlerror());
        return ESRCH;
    }

    if (!(*handle = dlopen(dli.dli_fname, RTLD_LAZY))) {
        sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                         "[%s] dlopen(%s) = %s",
                         func, dli.dli_fname, dlerror());
        return ESRCH;
    }

    dlinfo(*handle, RTLD_DI_LINKMAP, map);

    if (!map) {
        sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                         "[%s] dlinfo = %s",
                         func, dlerror());
        return ESRCH;
    }

    return SIGAR_OK;
}

int sigar_proc_list_get(sigar_t *sigar,
                        sigar_proc_list_t *proclist)
{
    return sigar_proc_list_procfs_get(sigar, proclist);
}

int sigar_proc_stat_get(sigar_t *sigar,
                        sigar_proc_stat_t *procstat)
{
    int status = /* XXX optimize */
        sigar_proc_count(sigar, &procstat->total);

    return status;
}

int sigar_proc_mem_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_mem_t *procmem)
{
    int status = sigar_proc_psinfo_get(sigar, pid);
    psinfo_t *pinfo = sigar->pinfo;
    prusage_t usage;

    if (status != SIGAR_OK) {
        return status;
    }

    procmem->size     = pinfo->pr_size << 10;
    procmem->resident = pinfo->pr_rssize << 10;
    procmem->share    = SIGAR_FIELD_NOTIMPL;

    if (sigar_proc_usage_get(sigar, &usage, pid) == SIGAR_OK) {
        procmem->minor_faults = usage.pr_minf;
        procmem->major_faults = usage.pr_majf;
        procmem->page_faults =
            procmem->minor_faults +
            procmem->major_faults;
    }
    else {
        procmem->minor_faults = SIGAR_FIELD_NOTIMPL;
        procmem->major_faults = SIGAR_FIELD_NOTIMPL;
        procmem->page_faults = SIGAR_FIELD_NOTIMPL;
    }

    return SIGAR_OK;
}

int sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_cred_t *proccred)
{
    int status = sigar_proc_psinfo_get(sigar, pid);
    psinfo_t *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    proccred->uid  = pinfo->pr_uid;
    proccred->gid  = pinfo->pr_gid;
    proccred->euid = pinfo->pr_euid;
    proccred->egid = pinfo->pr_egid;

    return SIGAR_OK;
}

#define TIMESTRUCT_2MSEC(t) \
    ((t.tv_sec * MILLISEC) + (t.tv_nsec / (NANOSEC/MILLISEC)))

int sigar_proc_time_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_time_t *proctime)
{
    prusage_t usage;
    int status;

    if ((status = sigar_proc_usage_get(sigar, &usage, pid)) != SIGAR_OK) {
        return status;
    }

    proctime->start_time = usage.pr_create.tv_sec + sigar->boot_time;
    proctime->start_time *= MILLISEC;

    if (usage.pr_utime.tv_sec < 0) {
        /* XXX wtf?  seen on solaris 10, only for the self process */
        pstatus_t pstatus;

        status = sigar_proc_status_get(sigar, &pstatus, pid);
        if (status != SIGAR_OK) {
            return status;
        }

        usage.pr_utime.tv_sec  = pstatus.pr_utime.tv_sec;
        usage.pr_utime.tv_nsec = pstatus.pr_utime.tv_nsec;
        usage.pr_stime.tv_sec  = pstatus.pr_stime.tv_sec;
        usage.pr_stime.tv_nsec = pstatus.pr_stime.tv_nsec;
    }

    proctime->user = TIMESTRUCT_2MSEC(usage.pr_utime);
    proctime->sys  = TIMESTRUCT_2MSEC(usage.pr_stime);
    proctime->total = proctime->user + proctime->sys;

    return SIGAR_OK;
}

int sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_state_t *procstate)
{
    int status = sigar_proc_psinfo_get(sigar, pid);
    psinfo_t *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    SIGAR_SSTRCPY(procstate->name, pinfo->pr_fname);
    procstate->ppid = pinfo->pr_ppid;
    procstate->tty  = pinfo->pr_ttydev;
    procstate->priority = pinfo->pr_lwp.pr_pri;
    procstate->nice     = pinfo->pr_lwp.pr_nice - NZERO;
    procstate->threads  = pinfo->pr_nlwp;
    procstate->processor = pinfo->pr_lwp.pr_onpro;

    switch (pinfo->pr_lwp.pr_state) {
      case SONPROC:
      case SRUN:
        procstate->state = 'R';
        break;
      case SZOMB:
        procstate->state = 'Z';
        break;
      case SSLEEP:
        procstate->state = 'S';
        break;
      case SSTOP:
        procstate->state = 'T';
        break;
      case SIDL:
        procstate->state = 'D';
        break;
    }

    return SIGAR_OK;
}

typedef struct {
    int timestamp;
    char *args;
} pargs_t;

static void pargs_free(void *value)
{
    pargs_t *pargs = (pargs_t *)value;
    if (pargs->args != NULL) {
        free(pargs->args);
    }
    free(pargs);
}

static int ucb_ps_args_get(sigar_t *sigar, sigar_pid_t pid,
                           sigar_proc_args_t *procargs,
                           int timestamp)
{
    char buffer[9086], *args=NULL, *arg;
    sigar_cache_entry_t *ent;
    FILE *fp;
    pargs_t *pargs;

    if (!sigar->pargs) {
        sigar->pargs = sigar_cache_new(15);
        sigar->pargs->free_value = pargs_free;
    }

    ent = sigar_cache_get(sigar->pargs, pid);
    if (ent->value) {
        pargs = (pargs_t *)ent->value;
        if (pargs->timestamp != timestamp) {
            if (pargs->args) {
                free(pargs->args);
                pargs->args = NULL;
            }
        }
    }
    else {
        pargs = malloc(sizeof(*pargs));
        pargs->args = NULL;
        ent->value = pargs;
    }

    pargs->timestamp = timestamp;

    if (pargs->args) {
        args = pargs->args;
    }
    else {
        snprintf(buffer, sizeof(buffer),
                 "/usr/ucb/ps -ww %ld", pid);

        if (!(fp = popen(buffer, "r"))) {
            return errno;
        }
        /* skip header */
        (void)fgets(buffer, sizeof(buffer), fp);
        if ((args = fgets(buffer, sizeof(buffer), fp))) {
            int len;

            /* skip PID,TT,S,TIME */
            args = sigar_skip_multiple_token(args, 4);
            SIGAR_SKIP_SPACE(args);
            len = strlen(args);
            if (len > 0) {
                args[len-1] = '\0'; /* chop \n */
            }

            pargs->args = malloc(len+1);
            memcpy(pargs->args, args, len);
        }

        pclose(fp);

        if (!args) {
            return ESRCH;
        }
    }

    sigar_proc_args_create(procargs);

    while (*args && (arg = sigar_getword(&args, ' '))) {
        SIGAR_PROC_ARGS_GROW(procargs);
        procargs->data[procargs->number++] = arg;
    }

    return SIGAR_OK;
}

int sigar_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_args_t *procargs)
{
    psinfo_t *pinfo;
    int fd, status;
    char buffer[9086];
    char *argvb[56];
    char **argvp = argvb;

    int n;
    size_t nread = 0;
    unsigned int argv_size;

    if ((status = sigar_proc_psinfo_get(sigar, pid)) != SIGAR_OK) {
        return status;
    }
    pinfo = sigar->pinfo;

    if (pinfo->pr_argc == 0) {
        procargs->number = procargs->size = 0;
        return SIGAR_OK;
    }
    else if (pinfo->pr_dmodel != PR_MODEL_NATIVE) {
        /* we are compiled in 32bit mode
         * punt any 64bit native process,
         * sizeof our structures can't handle.
         */
        if (sigar->use_ucb_ps) {
            return ucb_ps_args_get(sigar, pid, procargs,
                                   pinfo->pr_start.tv_sec);
        }
        else {
            return ENOTSUP;
        }
    }

    argv_size = sizeof(*argvp) * pinfo->pr_argc;

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/as");

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        if ((errno == EACCES) && sigar->use_ucb_ps) {
            return ucb_ps_args_get(sigar, pid, procargs,
                                   pinfo->pr_start.tv_sec);
        }
        else {
            return PROC_ERRNO;
        }
    }

    if (argv_size > sizeof(argvb)) {
        argvp = malloc(argv_size);
    }

    if ((nread = pread(fd, argvp, argv_size, pinfo->pr_argv)) <= 0) {
        close(fd);
        if (argvp != argvb) {
            free(argvp);
        }
        return errno;
    }

    procargs->number = 0;
    procargs->size = pinfo->pr_argc;
    procargs->data =
        (char **)malloc(sizeof(*(procargs->data)) *
                        procargs->size);

    for (n = 0; n < pinfo->pr_argc; n++) {
        int alen;
        char *arg;

        if ((nread = pread(fd, buffer, sizeof(buffer)-1, (off_t)argvp[n])) <= 0) {
            close(fd);
            if (argvp != argvb) {
                free(argvp);
            }
            sigar_proc_args_destroy(sigar, procargs);
            return errno;
        }

        buffer[nread] = '\0'; 
        alen = strlen(buffer)+1;
        arg = malloc(alen);
        memcpy(arg, buffer, alen);

        procargs->data[procargs->number++] = arg;
    }

    if (argvp != argvb) {
        free(argvp);
    }

    close(fd);

    return SIGAR_OK;
}

int sigar_proc_env_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_env_t *procenv)
{
    psinfo_t *pinfo;
    int fd, status;
    char buffer[BUFSIZ], *offsets[512];
    size_t nread;
    int n=0, max=sizeof(offsets)/sizeof(char *);

    if ((status = sigar_proc_psinfo_get(sigar, pid)) != SIGAR_OK) {
        return status;
    }
    pinfo = sigar->pinfo;

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/as");

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        return PROC_ERRNO;
    }

    if ((nread = pread(fd, offsets, sizeof(offsets),
                       pinfo->pr_envp)) <= 0)
    {
        close(fd);
        return errno;
    }

    while ((n < max) && offsets[n]) {
        char *val;
        int klen, vlen, status;
        char key[128]; /* XXX is there a max key size? */

        if ((nread = pread(fd, buffer, sizeof(buffer),
                           (off_t)offsets[n++])) <= 0)
        {
            close(fd);
            return errno;
        }

        val = strchr(buffer, '=');

        if (val == NULL) {
            break; /*XXX*/
        }

        klen = val - buffer;
        SIGAR_SSTRCPY(key, buffer);
        key[klen] = '\0';
        ++val;

        vlen = strlen(val);

        status = procenv->env_getter(procenv->data,
                                     key, klen, val, vlen);

        if (status != SIGAR_OK) {
            /* not an error; just stop iterating */
            break;
        }
    }

    close(fd);

    return SIGAR_OK;
}

int sigar_proc_fd_get(sigar_t *sigar, sigar_pid_t pid,
                      sigar_proc_fd_t *procfd)
{
    int status =
        sigar_proc_fd_count(sigar, pid, &procfd->total);

    return status;
}

static int sigar_proc_path_exe_get(sigar_t *sigar, sigar_pid_t pid,
                                   sigar_proc_exe_t *procexe)
{
    /* solaris 10+ */
    char buffer[BUFSIZ];

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/path/a.out");
    if (!proc_readlink(buffer, procexe->name, sizeof(procexe->name))) {
        procexe->name[0] = '\0';
    }

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/path/cwd");
    if (!proc_readlink(buffer, procexe->cwd, sizeof(procexe->cwd))) {
        procexe->cwd[0] = '\0';
    }

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/path/root");
    if (!proc_readlink(buffer, procexe->root, sizeof(procexe->root))) {
        procexe->root[0] = '\0';
    }

    return SIGAR_OK;
}

int sigar_proc_exe_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_exe_t *procexe)
{
    int status;
    char buffer[BUFSIZ];
    struct ps_prochandle *phandle;

    if (sigar->solaris_version >= 10) {
        return sigar_proc_path_exe_get(sigar, pid, procexe);
    }

    if ((status = sigar_init_libproc(sigar)) != SIGAR_OK) {
        return status;
    }

    procexe->name[0] = '\0';

    /* Pgrab would return G_SELF error */
    if (pid == sigar_pid_get(sigar)) {
        /* XXX: dunno if this will always work? */
        char *exe = getenv("_");
        if (exe) {
            SIGAR_STRNCPY(procexe->name, exe, sizeof(procexe->name));
        }
    }
    else {
        status = sigar_pgrab(sigar, pid, SIGAR_FUNC, &phandle);

        if (status == SIGAR_OK) {
            sigar->pexename(phandle, procexe->name, sizeof(procexe->name));
            sigar->pfree(phandle);
        }
    }

    if (procexe->name[0] == '\0') {
        /*XXX*/
    }

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/cwd");

    if (!sigar->pdirname(buffer, procexe->cwd, sizeof(procexe->cwd))) {
        procexe->cwd[0] = '\0';
    }

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/root");

    if (!(sigar->pdirname(buffer, procexe->root, sizeof(procexe->root)))) {
        procexe->root[0] = '\0';
    }

    return SIGAR_OK;
}

static int sigar_read_xmaps(sigar_t *sigar, 
                            prxmap_t *xmaps, int total,
                            unsigned long *last_inode,
                            struct ps_prochandle *phandle,
                            sigar_proc_modules_t *procmods)
{
    int status, i;
    unsigned long inode;
    char buffer[BUFSIZ];

    for (i=0; i<total; i++) {
        if (xmaps[i].pr_mflags & MA_ANON) {
            continue; /* heap, stack, etc */
        }

        inode = xmaps[i].pr_ino;

        if ((inode == 0) || (inode == *last_inode)) {
            *last_inode = 0;
            continue;
        }

        *last_inode = inode;

        sigar->pobjname(phandle, xmaps[i].pr_vaddr, buffer, sizeof(buffer));

        status = 
            procmods->module_getter(procmods->data, buffer, strlen(buffer));

        if (status != SIGAR_OK) {
            /* not an error; just stop iterating */
            return status;
        }
    }

    return SIGAR_OK;
}

static int sigar_pgrab_modules(sigar_t *sigar, sigar_pid_t pid,
                               sigar_proc_modules_t *procmods)
{
    int fd, pstatus;
    off_t map_size, nread;
    unsigned long last_inode = 0;
    prxmap_t xmaps[15]; /* ~2K */
    struct ps_prochandle *phandle;
    struct stat statbuf;
    char buffer[BUFSIZ];

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/xmap");

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        return errno;
    }

    if (fstat(fd, &statbuf) < 0) {
        close(fd);
        return errno;
    }

    map_size = statbuf.st_size;

    if (SIGAR_LOG_IS_DEBUG(sigar)) {
        sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                         "[%s] pid=%d, size=%d",
                         SIGAR_FUNC, pid, map_size);
    }

    if ((pstatus = sigar_init_libproc(sigar)) != SIGAR_OK) {
        close(fd);
        return pstatus;
    }

    pstatus = sigar_pgrab(sigar, pid, SIGAR_FUNC, &phandle);

    if (pstatus != SIGAR_OK) {
        close(fd);
        return pstatus;
    }

    for (nread=0; nread<statbuf.st_size; ) {
        off_t wanted = map_size > sizeof(xmaps) ? sizeof(xmaps) : map_size;
        int total = wanted / sizeof(prxmap_t);

        if (pread(fd, xmaps, wanted, nread) != wanted) {
            close(fd);
            sigar->pfree(phandle);
            return errno;
        }

        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "[%s] nread=%d, map_size=%d, wanted=%d, total=%d",
                             SIGAR_FUNC,
                             nread, map_size, wanted, total);
        }

        if (sigar_read_xmaps(sigar, xmaps, total,
                             &last_inode,
                             phandle, procmods) != SIGAR_OK)
        {
            break;
        }

        nread += wanted;
        map_size -= wanted;
    }

    close(fd);

    sigar->pfree(phandle);

    return SIGAR_OK;
}

static int sigar_dlinfo_modules(sigar_t *sigar, sigar_proc_modules_t *procmods)
{
    int status;
    void *handle;
    Link_map *map;

    status = sigar_dlinfo_get(sigar, SIGAR_FUNC, &handle, &map);
    if (status != SIGAR_OK) {
        return status;
    }

    do {
        int status = 
            procmods->module_getter(procmods->data,
                                    map->l_name, strlen(map->l_name));

        if (status != SIGAR_OK) {
            /* not an error; just stop iterating */
            return status;
        }
    } while ((map = map->l_next));

    dlclose(handle);

    return SIGAR_OK;
}

int sigar_proc_modules_get(sigar_t *sigar, sigar_pid_t pid,
                           sigar_proc_modules_t *procmods)
{
    if (pid == sigar_pid_get(sigar)) {
        /* Pgrab would return G_SELF, this is faster anyhow */
        /* XXX one difference to Pgrab, first entry is not the exe name */
        return sigar_dlinfo_modules(sigar, procmods);
    }
    else {
        return sigar_pgrab_modules(sigar, pid, procmods);
    }
}

#define TIME_NSEC(t) \
    (SIGAR_SEC2NANO((t).tv_sec) + (sigar_uint64_t)(t).tv_nsec)

int sigar_thread_cpu_get(sigar_t *sigar,
                         sigar_uint64_t id,
                         sigar_thread_cpu_t *cpu)
{
    struct lwpinfo info;

    if (id != 0) {
        return SIGAR_ENOTIMPL;
    }

    _lwp_info(&info);

    cpu->user  = TIME_NSEC(info.lwp_utime);
    cpu->sys   = TIME_NSEC(info.lwp_stime);
    cpu->total = TIME_NSEC(info.lwp_utime) + TIME_NSEC(info.lwp_stime);

    return SIGAR_OK;
}

#include <sys/mnttab.h>

int sigar_os_fs_type_get(sigar_file_system_t *fsp)
{
    char *type = fsp->sys_type_name;

    switch (*type) {
      case 'u':
        if (strEQ(type, "ufs")) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
        /* XXX */
    }

    return fsp->type;
}

int sigar_file_system_list_get(sigar_t *sigar,
                               sigar_file_system_list_t *fslist)
{
    struct mnttab ent;
    sigar_file_system_t *fsp;
    FILE *fp = fopen(MNTTAB, "r");

    if (!fp) {
        return errno;
    }

    sigar_file_system_list_create(fslist);

    while (getmntent(fp, &ent) == 0) {
        SIGAR_FILE_SYSTEM_LIST_GROW(fslist);

        fsp = &fslist->data[fslist->number++];

        SIGAR_SSTRCPY(fsp->dir_name, ent.mnt_mountp);
        SIGAR_SSTRCPY(fsp->dev_name, ent.mnt_special);
        SIGAR_SSTRCPY(fsp->sys_type_name, ent.mnt_fstype);
        sigar_fs_type_init(fsp);
    }

    fclose(fp);

    return SIGAR_OK;
}

#include <sys/statvfs.h>

#define SIGAR_FS_BLOCKS_TO_BYTES(buf, f) \
    (((sigar_uint64_t)buf.f * (buf.f_frsize / 512)) >> 1)

typedef struct {
    char device[PATH_MAX];
    char name[8];
    int instance;
} fsdev_path_t;

typedef struct {
    char module[8];
    int instance;
    char partition;
} fs_kstat_t;

static fsdev_path_t *get_fsdev_paths(sigar_t *sigar,
                                     sigar_file_system_list_t *fslist)
{
    int i, ndisk, size;
    char buffer[BUFSIZ], *ptr;
    char *dev, *inst, *drv;
    fsdev_path_t *paths, *mapping;
    FILE *fp = fopen("/etc/path_to_inst", "r");

    if (!fp) {
        return NULL;
    }

    for (i=0, ndisk=0; i<fslist->number; i++) {
        sigar_file_system_t *fsp = &fslist->data[i];
        if (fsp->type == SIGAR_FSTYPE_LOCAL_DISK) {
            ndisk++;
        }
    }

    size = sizeof(*paths) * (ndisk+1);
    mapping = paths = malloc(size);
    memset(mapping, '\0', size);

    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        /* eat dust java */
        char *q;

        SIGAR_SKIP_SPACE(ptr);
        if (*ptr == '#') {
            continue;
        }
        if (*ptr == '"') {
            ptr++;
        }
        dev = ptr;
        if (!(q = strchr(ptr, '"'))) {
            continue;
        }
        ptr = q+1;
        *q = '\0';
        SIGAR_SKIP_SPACE(ptr);
        inst = ptr;
        while (sigar_isdigit(*ptr)) {
            ptr++;
        }
        *ptr = '\0';
        ptr++;
        SIGAR_SKIP_SPACE(ptr);
        if (*ptr == '"') {
            ptr++;
        }
        drv = ptr;
        if (!(q = strchr(ptr, '"'))) {
            continue;
        }
        *q = '\0';

        if (!(strEQ(drv, "sd") ||
              strEQ(drv, "ssd") ||
              strEQ(drv, "st") ||
              strEQ(drv, "dad") ||
              strEQ(drv, "cmdk")))
        {
            continue;
        }

        paths->instance = atoi(inst);
        if (!kstat_lookup(sigar->kc, drv, paths->instance, NULL)) {
            continue;
        }

        SIGAR_SSTRCPY(paths->device, dev);
        SIGAR_SSTRCPY(paths->name, drv);

        if (--ndisk < 0) {
            /* XXX prevent overflow */
            break;
        }
        paths++;
    }
    fclose(fp);

    return mapping;
}

#define FSDEV_ID(sb) (sb.st_ino + sb.st_dev)

static int create_fsdev_cache(sigar_t *sigar)
{
    fsdev_path_t *paths, *mapping;
    sigar_file_system_list_t fslist;
    int i, j;
    int status;

    sigar->fsdev = sigar_cache_new(15);

    status = sigar_file_system_list_get(sigar, &fslist);
    
    if (status != SIGAR_OK) {
        return status;
    }

    if (!(mapping = get_fsdev_paths(sigar, &fslist))) {
        sigar_file_system_list_destroy(sigar, &fslist);
        return ENOENT;
    }

    for (i=0; i<fslist.number; i++) {
        sigar_file_system_t *fsp = &fslist.data[i];

        if (fsp->type == SIGAR_FSTYPE_LOCAL_DISK) {
            char device[PATH_MAX+1], *ptr=device;
            int len = readlink(fsp->dev_name, device, sizeof(device)-1);
            char *s;
            char partition;

            if (len < 0) {
                continue;
            }
            device[len] = '\0';
            while (strnEQ(ptr, "../", 3)) {
                ptr += 3;
            }
            if (strnEQ(ptr, "devices", 7)) {
                ptr += 7;
            }
            if ((s = strchr(ptr, ':'))) {
                partition = *(s+1);
            }
            else {
                continue;
            }

            for (j=0, paths=mapping; paths->name[0]; j++) {
                if (strnEQ(paths->device, ptr, strlen(paths->device))) {
                    sigar_cache_entry_t *ent;
                    struct stat sb;
                    int retval = stat(fsp->dir_name, &sb);
                    fs_kstat_t *fs_kstat;

                    if (retval == 0) {
                        fs_kstat = malloc(sizeof(*fs_kstat));

                        /* e.g. sd9,g
                         * module    == sd
                         * instance  == 9
                         * partition == 8
                         */
                        SIGAR_SSTRCPY(fs_kstat->module, paths->name);
                        fs_kstat->instance = paths->instance;
                        fs_kstat->partition = partition;
                        ent = sigar_cache_get(sigar->fsdev, FSDEV_ID(sb));
                        ent->value = fs_kstat;
                    }
                    break;
                }
                paths++;
            }
        }
    }

    free(mapping);
    sigar_file_system_list_destroy(sigar, &fslist);

    return SIGAR_OK;
}

static int fs_kstat_read(sigar_t *sigar,
                         sigar_file_system_usage_t *fsusage,
                         kstat_t *ksp)
{
    kstat_io_t *io;

    kstat_read(sigar->kc, ksp, NULL);

    io = (kstat_io_t *)ksp->ks_data;

    fsusage->disk_reads       = io->reads;
    fsusage->disk_writes      = io->writes;
    fsusage->disk_read_bytes  = io->nread;
    fsusage->disk_write_bytes = io->nwritten;
    fsusage->disk_queue       = io->wcnt; /* XXX ? */

    return SIGAR_OK;
}

static int get_fs_kstat(sigar_t *sigar,
                        sigar_file_system_usage_t *fsusage,
                        fs_kstat_t *fsk)
{
    kstat_t *ksp, *first;
    char *ptr;

    if (sigar_kstat_update(sigar) == -1) {
        return errno;
    }

    first = ksp =
        kstat_lookup(sigar->kc, fsk->module, fsk->instance, NULL);

    if (!ksp) {
        return ENXIO;
    }

    /* first entry is for the entire disk.
     * if there are no partitions specified,
     * report metrics for the entire disk.
     */
    if (!ksp->ks_next ||
        !strEQ(ksp->ks_next->ks_module, fsk->module))
    {
        return fs_kstat_read(sigar, fsusage, first);
    }
    ksp = ksp->ks_next;

    while (ksp) {
        if (!strEQ(ksp->ks_module, fsk->module)) {
            break; /* chain goes beyond our module */
        }
        if ((ptr = strchr(ksp->ks_name, ','))) {
            if (*(ptr+1) == fsk->partition) {
                return fs_kstat_read(sigar, fsusage, ksp);
            }
        }

        ksp = ksp->ks_next;
    }

    return ENOENT;
}

int sigar_file_system_usage_get(sigar_t *sigar,
                                const char *dirname,
                                sigar_file_system_usage_t *fsusage)
{
    struct statvfs buf;
    struct stat sb;
    sigar_cache_entry_t *ent;

    if (statvfs(dirname, &buf) != 0) {
        return errno;
    }

    fsusage->total = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_blocks);
    fsusage->free  = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_bfree);
    fsusage->avail = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_bavail);
    fsusage->used  = fsusage->total - fsusage->free;
    fsusage->files = buf.f_files;
    fsusage->free_files = buf.f_ffree;
    fsusage->use_percent = sigar_file_system_usage_calc_used(sigar, fsusage);

    SIGAR_DISK_STATS_NOTIMPL(fsusage);

    if (!sigar->fsdev) {
        if (create_fsdev_cache(sigar) != SIGAR_OK) {
            return SIGAR_OK;
        }
    }

    if (stat(dirname, &sb) < 0) {
        return SIGAR_OK;
    }
    ent = sigar_cache_get(sigar->fsdev, FSDEV_ID(sb));
    if (ent->value == NULL) {
        return SIGAR_OK;
    }

    get_fs_kstat(sigar, fsusage, (fs_kstat_t *)ent->value);

    return SIGAR_OK;
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    processor_info_t stats;
    unsigned int i;
    int status = SIGAR_OK;
    int brand = -1;

    if (sigar_kstat_update(sigar) == -1) { /* for sigar->ncpu */
        return errno;
    }

    /*
     * stats we care about will be the same for each
     * online processor, so just grab the first.
     */
    for (i=0; i<sigar->ncpu; i++) {
        processorid_t id = sigar->ks.cpuid[i];

        if ((status = processor_info(id, &stats)) < 0) {
            continue;
        }
        else {
            status = SIGAR_OK;
            break;
        }
    }

    if (status != SIGAR_OK) {
        /* should never happen */
        return ENOENT;
    }

    sigar_cpu_info_list_create(cpu_infos);

    for (i=0; i<sigar->ncpu; i++) {
        sigar_cpu_info_t *info;

        if (is_same_chip(sigar, i, cpu_infos->number)) {
            continue;
        }

        SIGAR_CPU_INFO_LIST_GROW(cpu_infos);

        info = &cpu_infos->data[cpu_infos->number++];

        SIGAR_SSTRCPY(info->model, stats.pi_processor_type);

        if (brand == -1) {
            brand = get_chip_brand(sigar, i, info);
        }

        if (strEQ(info->model, "i386")) {
            if (!brand) {
                /* assume Intel on x86 */
                SIGAR_SSTRCPY(info->vendor, "Intel");
            }
            SIGAR_SSTRCPY(info->model, "x86");
        }
        else {
            if (!brand) {
                /* assume Sun */
                SIGAR_SSTRCPY(info->vendor, "Sun");
            }
            /* s/sparc/Sparc/ */
            info->model[0] = toupper(info->model[0]);
        }

        if (brand) {
            SIGAR_SSTRCPY(info->vendor, cpu_infos->data[0].vendor);
        }

        info->mhz = stats.pi_clock;
        info->cache_size = SIGAR_FIELD_NOTIMPL; /*XXX*/
    }

    return SIGAR_OK;
}

int sigar_net_route_list_get(sigar_t *sigar,
                             sigar_net_route_list_t *routelist)

{
    char *data;
    int len, rc;
    struct opthdr *op;
    size_t nread=0, size=sizeof(mib2_ipRouteEntry_t);

    if (sigar->solaris_version >= 10) {
        size += /* bincompat for new solaris 10 fields */
            sizeof(DeviceName) +
            sizeof(IpAddress);
    }

    sigar_net_route_list_create(routelist);

    while ((rc = get_mib2(&sigar->mib2, &op, &data, &len)) == GET_MIB2_OK) {
        mib2_ipRouteEntry_t *entry;
        char *end;

        if (!((op->level == MIB2_IP) && (op->name == MIB2_IP_21))) {
            continue;
        }

        for (entry = (mib2_ipRouteEntry_t *)data, end = data + len;
             (char *)entry < end;
             entry = (mib2_ipRouteEntry_t *)((char *)data+nread), nread+=size)
        {
            sigar_net_route_t *route;
            int type = entry->ipRouteInfo.re_ire_type;

            /* filter same as netstat -r */
            if ((type == IRE_CACHE) ||
                (type == IRE_BROADCAST) ||
                (type == IRE_LOCAL))
            {
                continue;
            }

            SIGAR_NET_ROUTE_LIST_GROW(routelist);
            route = &routelist->data[routelist->number++];

            sigar_net_address_set(route->destination,
                                  entry->ipRouteDest);

            sigar_net_address_set(route->gateway,
                                  entry->ipRouteNextHop);

            sigar_net_address_set(route->mask,
                                  entry->ipRouteMask);

            route->refcnt      = entry->ipRouteInfo.re_ref;
            route->irtt        = entry->ipRouteInfo.re_rtt;
            route->metric      = entry->ipRouteMetric1;

            SIGAR_SSTRCPY(route->ifname, entry->ipRouteIfIndex.o_bytes);

            route->flags = RTF_UP;
            if ((route->destination.addr.in == 0) &&
                (route->mask.addr.in == 0))
            {
                route->flags |= RTF_GATEWAY;
            }

            route->use = route->window = route->mtu = 
                SIGAR_FIELD_NOTIMPL; /*XXX*/
        }
    }

    if (rc != GET_MIB2_EOD) {
        close_mib2(&sigar->mib2);
        return SIGAR_EMIB2;
    }

    return SIGAR_OK;
}

#define kHME(v) kSTAT_uint(v, hme)

static int sigar_net_ifstat_get_hme(sigar_t *sigar, const char *name,
                                    sigar_net_interface_stat_t *ifstat)
{
    kstat_ctl_t *kc = sigar->kc; 
    kstat_t *ksp;
    int status;

    status = sigar_get_multi_kstats(sigar, &sigar->ks.hme,
                                    name, &ksp);

    if (status != SIGAR_OK) {
        return status;
    }

    kstat_read(kc, ksp, NULL);
    
    sigar_koffsets_init_hme(sigar, ksp);

    ifstat->rx_packets    = kHME(KSTAT_HME_RX_PACKETS);
    ifstat->rx_bytes      = kHME(KSTAT_HME_RX_BYTES);
    ifstat->rx_errors     = kHME(KSTAT_HME_RX_ERRORS);
    ifstat->rx_dropped    = kHME(KSTAT_HME_RX_DROPPED); /*XXX*/
    ifstat->rx_overruns   = kHME(KSTAT_HME_RX_OVERRUNS); /*XXX*/
    ifstat->rx_frame      = kHME(KSTAT_HME_RX_FRAME);

    ifstat->tx_packets    = kHME(KSTAT_HME_TX_PACKETS);
    ifstat->tx_bytes      = kHME(KSTAT_HME_TX_BYTES);
    ifstat->tx_errors     = kHME(KSTAT_HME_TX_ERRORS);
    ifstat->tx_dropped    = kHME(KSTAT_HME_TX_DROPPED); /*XXX*/
    ifstat->tx_overruns   = kHME(KSTAT_HME_TX_OVERRUNS); /*XXX*/
    ifstat->tx_collisions = kHME(KSTAT_HME_TX_COLLISIONS);
    ifstat->tx_carrier    = kHME(KSTAT_HME_TX_CARRIER);
    ifstat->speed         = kHME(KSTAT_HME_SPEED);

    return SIGAR_OK;
}

#define kDMFE(v) kSTAT_uint(v, dmfe)

static int sigar_net_ifstat_get_dmfe(sigar_t *sigar, const char *name,
                                    sigar_net_interface_stat_t *ifstat)
{
    kstat_ctl_t *kc = sigar->kc; 
    kstat_t *ksp;
    int status;

    status = sigar_get_multi_kstats(sigar, &sigar->ks.dmfe,
                                    name, &ksp);

    if (status != SIGAR_OK) {
        return status;
    }

    kstat_read(kc, ksp, NULL);
    
    sigar_koffsets_init_dmfe(sigar, ksp);

    ifstat->rx_packets    = kDMFE(KSTAT_DMFE_RX_PACKETS);
    ifstat->rx_bytes      = kDMFE(KSTAT_DMFE_RX_BYTES);
    ifstat->rx_errors     = kDMFE(KSTAT_DMFE_RX_ERRORS);
    ifstat->rx_dropped    = kDMFE(KSTAT_DMFE_RX_DROPPED); /*XXX*/
    ifstat->rx_overruns   = kDMFE(KSTAT_DMFE_RX_OVERRUNS); /*XXX*/
    ifstat->rx_frame      = kDMFE(KSTAT_DMFE_RX_FRAME);

    ifstat->tx_packets    = kDMFE(KSTAT_DMFE_TX_PACKETS);
    ifstat->tx_bytes      = kDMFE(KSTAT_DMFE_TX_BYTES);
    ifstat->tx_errors     = kDMFE(KSTAT_DMFE_TX_ERRORS);
    ifstat->tx_dropped    = kDMFE(KSTAT_DMFE_TX_DROPPED); /*XXX*/
    ifstat->tx_overruns   = kDMFE(KSTAT_DMFE_TX_OVERRUNS); /*XXX*/
    ifstat->tx_collisions = kDMFE(KSTAT_DMFE_TX_COLLISIONS);
    ifstat->tx_carrier    = kDMFE(KSTAT_DMFE_TX_CARRIER);

    ifstat->speed         = kDMFE(KSTAT_DMFE_SPEED);

    return SIGAR_OK;
}

#define kGE(v) kSTAT_uint(v, ge)

static int sigar_net_ifstat_get_ge(sigar_t *sigar, const char *name,
                                   sigar_net_interface_stat_t *ifstat)
{
    kstat_ctl_t *kc = sigar->kc; 
    kstat_t *ksp;
    int status;

    status = sigar_get_multi_kstats(sigar, &sigar->ks.ge,
                                    name, &ksp);

    if (status != SIGAR_OK) {
        return status;
    }

    kstat_read(kc, ksp, NULL);
    
    sigar_koffsets_init_ge(sigar, ksp);

    ifstat->rx_packets    = kGE(KSTAT_GE_RX_PACKETS);
    ifstat->rx_bytes      = kGE(KSTAT_GE_RX_BYTES);
    ifstat->rx_errors     = kGE(KSTAT_GE_RX_ERRORS);
    ifstat->rx_dropped    = kGE(KSTAT_GE_RX_DROPPED); /*XXX*/
    ifstat->rx_overruns   = kGE(KSTAT_GE_RX_OVERRUNS); /*XXX*/
    ifstat->rx_frame      = kGE(KSTAT_GE_RX_FRAME);

    ifstat->tx_packets    = kGE(KSTAT_GE_TX_PACKETS);
    ifstat->tx_bytes      = kGE(KSTAT_GE_TX_BYTES);
    ifstat->tx_errors     = kGE(KSTAT_GE_TX_ERRORS);
    ifstat->tx_dropped    = kGE(KSTAT_GE_TX_DROPPED); /*XXX*/
    ifstat->tx_overruns   = kGE(KSTAT_GE_TX_OVERRUNS); /*XXX*/
    ifstat->tx_collisions = kGE(KSTAT_GE_TX_COLLISIONS);
    ifstat->tx_carrier    = kGE(KSTAT_GE_TX_CARRIER);

    ifstat->speed         = kGE(KSTAT_GE_SPEED);

    return SIGAR_OK;
}

#define kERI(v) kSTAT_uint(v, eri)

static int sigar_net_ifstat_get_eri(sigar_t *sigar, const char *name,
                                    sigar_net_interface_stat_t *ifstat)
{
    kstat_ctl_t *kc = sigar->kc; 
    kstat_t *ksp;
    int status;

    status = sigar_get_multi_kstats(sigar, &sigar->ks.eri,
                                    name, &ksp);

    if (status != SIGAR_OK) {
        return status;
    }

    kstat_read(kc, ksp, NULL);
    
    sigar_koffsets_init_eri(sigar, ksp);

    ifstat->rx_packets    = kERI(KSTAT_ERI_RX_PACKETS);
    ifstat->rx_bytes      = kERI(KSTAT_ERI_RX_BYTES);
    ifstat->rx_errors     = kERI(KSTAT_ERI_RX_ERRORS);
    ifstat->rx_dropped    = kERI(KSTAT_ERI_RX_DROPPED); /*XXX*/
    ifstat->rx_overruns   = kERI(KSTAT_ERI_RX_OVERRUNS); /*XXX*/
    ifstat->rx_frame      = kERI(KSTAT_ERI_RX_FRAME);

    ifstat->tx_packets    = kERI(KSTAT_ERI_TX_PACKETS);
    ifstat->tx_bytes      = kERI(KSTAT_ERI_TX_BYTES);
    ifstat->tx_errors     = kERI(KSTAT_ERI_TX_ERRORS);
    ifstat->tx_dropped    = kERI(KSTAT_ERI_TX_DROPPED); /*XXX*/
    ifstat->tx_overruns   = kERI(KSTAT_ERI_TX_OVERRUNS); /*XXX*/
    ifstat->tx_collisions = kERI(KSTAT_ERI_TX_COLLISIONS);
    ifstat->tx_carrier    = kERI(KSTAT_ERI_TX_CARRIER);

    ifstat->speed         = kERI(KSTAT_ERI_SPEED);

    return SIGAR_OK;
}

#define kLO(v) kSTAT_uint(v, lo)

#define jLO aHO

static int sigar_net_ifstat_get_lo(sigar_t *sigar, const char *name,
                                   sigar_net_interface_stat_t *ifstat)
{
    kstat_ctl_t *kc = sigar->kc; 
    kstat_t *ksp;
    int status;

    status = sigar_get_multi_kstats(sigar, &sigar->ks.lo,
                                    name, &ksp);

    if (status != SIGAR_OK) {
        return status;
    }

    kstat_read(kc, ksp, NULL);
    
    sigar_koffsets_init_lo(sigar, ksp);

    ifstat->rx_packets    = kLO(KSTAT_LO_RX_PACKETS);
    ifstat->rx_bytes      = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_errors     = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_dropped    = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_overruns   = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_frame      = SIGAR_FIELD_NOTIMPL;

    ifstat->tx_packets    = kLO(KSTAT_LO_TX_PACKETS);
    ifstat->tx_bytes      = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_errors     = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_dropped    = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_overruns   = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_collisions = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_carrier    = SIGAR_FIELD_NOTIMPL;

    ifstat->speed         = SIGAR_FIELD_NOTIMPL;

    return SIGAR_OK;
}

static void ifstat_kstat_common(sigar_net_interface_stat_t *ifstat,
                                kstat_named_t *data, int ndata)
{
    int i;

    for (i=0; i<ndata; i++) {
        sigar_uint64_t value = data[i].value.ui32;

        char *ptr = data[i].name;

        switch (*ptr) {
          case 'c':
            if (strEQ(ptr, "collisions")) {
                ifstat->tx_collisions = value;
            }
            break;
          case 'd':
            if (strEQ(ptr, "drop")) {
                ifstat->rx_dropped = value;
                ifstat->tx_dropped = value;
            }
            break;
          case 'i':
            if (strEQ(ptr, "ipackets")) {
                ifstat->rx_packets = value;
            }
            else if (strEQ(ptr, "ierrors")) {
                ifstat->rx_errors = value;
            }
            else if (strEQ(ptr, "ifspeed")) {
                ifstat->speed = value;
            }
            break;
          case 'f':
            if (strEQ(ptr, "framing")) {
                ifstat->rx_frame = value;
            }
            break;
          case 'm':
            if (strEQ(ptr, "missed")) {
                ifstat->rx_dropped = value;
                ifstat->tx_dropped = value;
            }
            break;
          case 'n':
            if (strEQ(ptr, "nocarrier")) {
                ifstat->tx_carrier = value;
            }
            break;
          case 'o':
            if (strEQ(ptr, "obytes")) {
                ifstat->tx_bytes = value;
            }
            else if (strEQ(ptr, "oerrors")) {
                ifstat->tx_errors = value;
            }
            else if (strEQ(ptr, "oflo")) {
                ifstat->tx_overruns = value;
            }
            else if (strEQ(ptr, "opackets")) {
                ifstat->tx_packets = value;
            }
            else if (strEQ(ptr, "toolong_errors")) {
                ifstat->tx_overruns = value;
            }
            break;
          case 'r':
            if (strEQ(ptr, "rbytes")) {
                ifstat->rx_bytes = value;
            }
            else if (strEQ(ptr, "rx_overflow")) {
                ifstat->rx_overruns = value;
            }
            break;
          default:
            break;
        }
    }
}

static int sigar_net_ifstat_get_any(sigar_t *sigar, const char *name,
                                    sigar_net_interface_stat_t *ifstat)
{
    kstat_ctl_t *kc = sigar->kc; 
    kstat_t *ksp;
    kstat_named_t *data;
    char dev[64], *ptr=dev;
    int num;

    if (sigar_kstat_update(sigar) == -1) {
        return errno;
    }

    strncpy(dev, name, sizeof(dev)-1);
    dev[sizeof(dev)-1] = '\0';

    while (!sigar_isdigit(*ptr) && (*ptr != '\0')) {
        ptr++;
    }

    if (*ptr == '\0') {
        return ENXIO;
    }

    /* iprb0 -> dev="iprb", num=0 */
    num = atoi(ptr);
    *ptr = '\0';

    if (!(ksp = kstat_lookup(kc, dev, num, (char *)name))) {
        return ENXIO;
    }

    if (kstat_read(kc, ksp, NULL) < 0) {
        return ENOENT;
    }

    SIGAR_ZERO(ifstat);

    data = (kstat_named_t *)ksp->ks_data;

    ifstat_kstat_common(ifstat, data, ksp->ks_ndata);

    return SIGAR_OK;
}

int sigar_net_interface_stat_get(sigar_t *sigar, const char *name,
                                 sigar_net_interface_stat_t *ifstat)
{
    ifstat->speed = SIGAR_FIELD_NOTIMPL;

    switch (*name) {
      case 'd':
        if (strnEQ(name, "dmfe", 4)) {
            return sigar_net_ifstat_get_dmfe(sigar, name, ifstat);
        }
        break;
      case 'e':
        if (strnEQ(name, "eri", 3)) {
            return sigar_net_ifstat_get_eri(sigar, name, ifstat);
        }
        break;
      case 'g':
        if (strnEQ(name, "ge", 2)) {
            return sigar_net_ifstat_get_ge(sigar, name, ifstat);
        }
        break;
      case 'h':
        if (strnEQ(name, "hme", 3)) {
            return sigar_net_ifstat_get_hme(sigar, name, ifstat);
        }
        break;
      case 'l':
        if (strnEQ(name, "lo", 2)) {
            return sigar_net_ifstat_get_lo(sigar, name, ifstat);
        }
        break;
      default:
        break;
    }
    
    return sigar_net_ifstat_get_any(sigar, name, ifstat);
}

#define TCPQ_SIZE(s) ((s) >= 0 ? (s) : 0)

static int tcp_connection_get(sigar_net_connection_walker_t *walker,
                              struct mib2_tcpConnEntry *entry,
                              int len)
{
    int flags = walker->flags;
    int status;
    char *end = (char *)entry + len;

    while ((char *)entry < end) {
        int state = entry->tcpConnEntryInfo.ce_state;

        if (((flags & SIGAR_NETCONN_SERVER) && (state == TCPS_LISTEN)) ||
            ((flags & SIGAR_NETCONN_CLIENT) && (state != TCPS_LISTEN)))
        {
            sigar_net_connection_t conn;

            SIGAR_ZERO(&conn);

            sigar_net_address_set(conn.local_address, entry->tcpConnLocalAddress);
            sigar_net_address_set(conn.remote_address, entry->tcpConnRemAddress);

            conn.local_port = entry->tcpConnLocalPort;
            conn.remote_port = entry->tcpConnRemPort;
            conn.type = SIGAR_NETCONN_TCP;
            conn.send_queue =
                TCPQ_SIZE(entry->tcpConnEntryInfo.ce_snxt -
                          entry->tcpConnEntryInfo.ce_suna - 1);
            conn.receive_queue =
                TCPQ_SIZE(entry->tcpConnEntryInfo.ce_rnxt -
                          entry->tcpConnEntryInfo.ce_rack);

            switch (state) {
              case TCPS_CLOSED:
                conn.state = SIGAR_TCP_CLOSE;
                break;
              case TCPS_IDLE:
                conn.state = SIGAR_TCP_IDLE;
                break;
              case TCPS_BOUND:
                conn.state = SIGAR_TCP_BOUND;
                break;
              case TCPS_LISTEN:
                conn.state = SIGAR_TCP_LISTEN;
                break;
              case TCPS_SYN_SENT:
                conn.state = SIGAR_TCP_SYN_SENT;
                break;
              case TCPS_SYN_RCVD:
                conn.state = SIGAR_TCP_SYN_RECV;
                break;
              case TCPS_ESTABLISHED:
                conn.state = SIGAR_TCP_ESTABLISHED;
                break;
              case TCPS_CLOSE_WAIT:
                conn.state = SIGAR_TCP_CLOSE_WAIT;
                break;
              case TCPS_FIN_WAIT_1:
                conn.state = SIGAR_TCP_FIN_WAIT1;
                break;
              case TCPS_CLOSING:
                conn.state = SIGAR_TCP_CLOSING;
                break;
              case TCPS_LAST_ACK:
                conn.state = SIGAR_TCP_LAST_ACK;
                break;
              case TCPS_FIN_WAIT_2:
                conn.state = SIGAR_TCP_FIN_WAIT2;
                break;
              case TCPS_TIME_WAIT:
                conn.state = SIGAR_TCP_TIME_WAIT;
                break;
              default:
                conn.state = SIGAR_TCP_UNKNOWN;
                break;
            }

            status = walker->add_connection(walker, &conn);
            if (status != SIGAR_OK) {
                return status;
            }
        }

        entry++;
    }

    return SIGAR_OK;
}

static int udp_connection_get(sigar_net_connection_walker_t *walker,
                              struct mib2_udpEntry *entry,
                              int len)
{
    int flags = walker->flags;
    int status;
    char *end = (char *)entry + len;

    while ((char *)entry < end) {
        int state = entry->udpEntryInfo.ue_state;

        /* XXX dunno if this state check is right */
        if (((flags & SIGAR_NETCONN_SERVER) && (state == MIB2_UDP_idle)) ||
            ((flags & SIGAR_NETCONN_CLIENT) && (state != MIB2_UDP_idle)))
        {
            sigar_net_connection_t conn;

            SIGAR_ZERO(&conn);

            sigar_net_address_set(conn.local_address, entry->udpLocalAddress);
            sigar_net_address_set(conn.remote_address, 0);

            conn.local_port = entry->udpLocalPort;
            conn.remote_port = 0;
            conn.type = SIGAR_NETCONN_UDP;

            status = walker->add_connection(walker, &conn);
            if (status != SIGAR_OK) {
                return status;
            }
        }

        entry++;
    }

    return SIGAR_OK;
}

int sigar_net_connection_walk(sigar_net_connection_walker_t *walker)
{
    sigar_t *sigar = walker->sigar;
    int flags = walker->flags;
    int status;
    int want_tcp = flags & SIGAR_NETCONN_TCP;
    int want_udp = flags & SIGAR_NETCONN_UDP;
    char *data;
    int len;
    int rc;
    struct opthdr *op;

    while ((rc = get_mib2(&sigar->mib2, &op, &data, &len)) == GET_MIB2_OK) {
        if ((op->level == MIB2_TCP) && 
            (op->name == MIB2_TCP_13) &&
            want_tcp)
        {
            status =
                tcp_connection_get(walker,
                                   (struct mib2_tcpConnEntry *)data,
                                   len);
        }
        else if ((op->level == MIB2_UDP) && 
                 (op->name == MIB2_UDP_5) &&
                 want_udp)
        {
            status =
                udp_connection_get(walker,
                                   (struct mib2_udpEntry *)data,
                                   len);
        }
        else {
            status = SIGAR_OK;
        }

        if (status != SIGAR_OK) {
            break;
        }
    }

    if (rc != GET_MIB2_EOD) {
        close_mib2(&sigar->mib2);
        return SIGAR_EMIB2;
    }

    return SIGAR_OK;
}

int sigar_proc_port_get(sigar_t *sigar, int protocol,
                        unsigned long port, sigar_pid_t *pid)
{
    return SIGAR_ENOTIMPL;
}

int sigar_os_sys_info_get(sigar_t *sigar,
                          sigar_sys_info_t *sys_info)
{
    char *vendor_version;

    sysinfo(SI_ARCHITECTURE, sys_info->arch, sizeof(sys_info->arch));

    SIGAR_SSTRCPY(sys_info->name, "Solaris");
    SIGAR_SSTRCPY(sys_info->vendor, "Sun Microsystems");

    if (strEQ(sys_info->version, "5.6")) {
        vendor_version = "2.6";
    }
    else {
        if ((vendor_version = strchr(sys_info->version, '.'))) {
            ++vendor_version;
        }
        else {
            vendor_version = sys_info->version;
        }
    }

    SIGAR_SSTRCPY(sys_info->vendor_version, vendor_version);

    snprintf(sys_info->description,
             sizeof(sys_info->description),
             "%s %s",
             sys_info->name, sys_info->vendor_version);

    return SIGAR_OK;
}
