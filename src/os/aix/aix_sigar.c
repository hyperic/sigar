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

/* pull in time.h before resource.h does w/ _KERNEL */
#include <sys/time.h>
#define _KERNEL 1 
#include <sys/file.h>     /* for struct file */
#include <sys/resource.h> /* for rlimit32 in 64-bit mode */
#undef  _KERNEL

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

#include <dlfcn.h>
#include <nlist.h>
#include <pthread.h>
#include <stdio.h>
#include <utmp.h>

#include <sys/statfs.h>
#include <sys/systemcfg.h>
#include <sys/sysinfo.h>
#include <sys/var.h>
#include <sys/vminfo.h>
#include <sys/mntctl.h>
#include <sys/stat.h>
#include <sys/user.h>
#include <sys/utsname.h>
#include <sys/vmount.h>
#include <sys/proc.h>

#include <sys/socket.h>
#include <arpa/inet.h>
#include <net/if.h>

/* for proc_port */
#include <netinet/in_pcb.h>
#include <sys/domain.h>
#include <sys/protosw.h>
#include <sys/socketvar.h>

/* for net_connection_list */
#include <netinet/ip_var.h>
#include <netinet/tcp_timer.h>
#include <netinet/tcp_var.h>
#include <netinet/tcp_fsm.h>

/* for odm api */
#include <sys/cfgodm.h>
#include <sys/cfgdb.h>
#include <cf.h>

#include <sys/ldr.h>

/* not defined in aix 4.3 */
#ifndef SBITS
#define SBITS 16
#endif

#ifndef PTHRDSINFO_RUSAGE_START
#define PTHRDSINFO_RUSAGE_START   0x00000001
#define PTHRDSINFO_RUSAGE_STOP    0x00000002
#define PTHRDSINFO_RUSAGE_COLLECT 0x00000004
#endif

/*
 * from libperfstat.h:
 * "To calculate the load average, divide the numbers by (1<<SBITS).
 *  SBITS is defined in <sys/proc.h>."
 */
#define FIXED_TO_DOUBLE(x) (((double)x) / (1<<SBITS))

/* these offsets wont change so just lookup them up during open */
static int get_koffsets(sigar_t *sigar)
{
    int i;
    /* see man knlist and nlist.h */
    struct nlist klist[] = {
        {"avenrun", 0, 0, 0, 0, 0}, /* KOFFSET_LOADAVG */
        {"v", 0, 0, 0, 0, 0}, /* KOFFSET_VAR */
        {"sysinfo", 0, 0, 0, 0, 0}, /* KOFFSET_SYSINFO */
        {"ifnet", 0, 0, 0, 0, 0}, /* KOFFSET_IFNET */
        {"vmminfo", 0, 0, 0, 0, 0}, /* KOFFSET_VMINFO */
        {"cpuinfo", 0, 0, 0, 0, 0}, /* KOFFSET_CPUINFO */
        {"tcb", 0, 0, 0, 0, 0}, /* KOFFSET_TCB */
        {NULL, 0, 0, 0, 0, 0}
    };

    if (knlist(klist,
               sizeof(klist) / sizeof(klist[0]),
               sizeof(klist[0])) != 0)
    {
        return errno;
    }

    for (i=0; i<KOFFSET_MAX; i++) {
        sigar->koffsets[i] = klist[i].n_value;
    }
    
    return SIGAR_OK;
}

static int kread(sigar_t *sigar, void *data, int size, long offset)
{
    if (sigar->kmem < 0) {
        return SIGAR_EPERM_KMEM;
    }

    if (lseek(sigar->kmem, offset, SEEK_SET) != offset) {
        return errno;
    }

    if (read(sigar->kmem, data, size) != size) {
        return errno;
    }

    return SIGAR_OK;
}

int sigar_os_open(sigar_t **sigar)
{
    int status, i;
    int kmem = -1;
    struct utsname name;

    kmem = open("/dev/kmem", O_RDONLY);

    *sigar = malloc(sizeof(**sigar));

    (*sigar)->getprocfd = NULL; /*XXX*/
    (*sigar)->kmem = kmem;
    (*sigar)->dmem = -1;
    (*sigar)->pagesize = 0;
    (*sigar)->ticks = sysconf(_SC_CLK_TCK);
    (*sigar)->boot_time = 0;
    (*sigar)->last_pid = -1;
    (*sigar)->pinfo = NULL;
    (*sigar)->cpuinfo = NULL;
    (*sigar)->cpuinfo_size = 0;
    SIGAR_ZERO(&(*sigar)->swaps);
    SIGAR_ZERO(&(*sigar)->perfstat);

    i = getpagesize();
    while ((i >>= 1) > 0) {
        (*sigar)->pagesize++;
    }

    if (kmem > 0) {
        if ((status = get_koffsets(*sigar)) != SIGAR_OK) {
            free(*sigar);
            return status;
        }
    }

    (*sigar)->cpu_mhz = -1;

    (*sigar)->model[0] = '\0';

    uname(&name);

    (*sigar)->aix_version = atoi(name.version);

    (*sigar)->thrusage = PTHRDSINFO_RUSAGE_STOP;

    (*sigar)->diskmap = NULL;

    return SIGAR_OK;
}

static void swaps_free(swaps_t *swaps);

int sigar_os_close(sigar_t *sigar)
{
    swaps_free(&sigar->swaps);
    if (sigar->kmem > 0) {
        close(sigar->kmem);
    }
    if (sigar->dmem > 0) {
        close(sigar->dmem);
    }
    if (sigar->pinfo) {
        free(sigar->pinfo);
    }
    if (sigar->cpuinfo) {
        free(sigar->cpuinfo);
    }
    if (sigar->perfstat.handle) {
        dlclose(sigar->perfstat.handle);
    }
    if (sigar->diskmap) {
        sigar_cache_destroy(sigar->diskmap);
    }
    if (sigar->thrusage == PTHRDSINFO_RUSAGE_START) {
        struct rusage usage;
        sigar->perfstat.thread_rusage(&usage,
                                      PTHRDSINFO_RUSAGE_STOP);
    }
    free(sigar);
    return SIGAR_OK;
}

char *sigar_os_error_string(sigar_t *sigar, int err)
{
    switch (err) {
      case SIGAR_EPERM_KMEM:
        return "Failed to open /dev/kmem for reading";
      default:
        return NULL;
    }
}

static int proc_module_get_self(void *data, char *name, int len)
{
    sigar_t *sigar = (sigar_t *)data;
    char *ptr = rindex(name, '/');

    if (!ptr) {
        return SIGAR_OK;
    }

    if (strnEQ(ptr+1, "libsigar-", 9)) {
        *ptr = '\0'; /* chop libsigar-powerpc-ibm-aix-4.3.x.so */

        sigar->self_path = sigar_strdup(name);

        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "detected sigar-lib='%s'",
                             sigar->self_path);
        }

        return !SIGAR_OK; /* break loop */
    }

    return SIGAR_OK;
}

static char *sigar_get_self_path(sigar_t *sigar)
{
    if (!sigar->self_path) {
        sigar_proc_modules_t procmods;
        procmods.module_getter = proc_module_get_self;
        procmods.data = sigar;

        sigar_proc_modules_get(sigar, sigar_pid_get(sigar),
                               &procmods);

        if (!sigar->self_path) {
            /* dont try again */
            sigar->self_path = sigar_strdup(".");
        }
    }

    return sigar->self_path;
}

/*
 * the perfstat api is only supported in aix 5.2+
 * in order to be binary compatible with 4.3 and 5.1
 * we must jump through some hoops.  libperfstat.a
 * is a static library, we need dynamic.
 * libsigar_aixperfstat.so is juat a proxy to libperfstat.a
 */
#define SIGAR_AIXPERFSTAT "/libsigar_aixperfstat.so"

static int sigar_perfstat_init(sigar_t *sigar)
{
    void *handle;
    char libperfstat[SIGAR_PATH_MAX+1], *path;
    int len;

    if (sigar->perfstat.avail == 1) {
        return SIGAR_OK;
    }
    if (sigar->perfstat.avail == -1) {
        return ENOENT;
    }

    path = sigar_get_self_path(sigar);
    len = strlen(path);

    memcpy(&libperfstat[0], path, len);
    memcpy(&libperfstat[len], SIGAR_AIXPERFSTAT, 
           sizeof(SIGAR_AIXPERFSTAT));

    if (!(handle = dlopen(libperfstat, RTLD_LOCAL|RTLD_LAZY))) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "failed to open '%s': %s",
                             libperfstat, sigar_strerror(sigar, errno));
        }

        sigar->perfstat.avail = -1;
        return errno;
    }

    sigar->perfstat.thread_rusage =
        (thread_rusage_func_t)dlsym(handle,
                                    "sigar_thread_rusage");

    if (!sigar->perfstat.thread_rusage) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "dlsym(sigar_thread_rusage) failed: %s",
                             dlerror());
        }
    }

    sigar->perfstat.cpu_total =
        (perfstat_cpu_total_func_t)dlsym(handle,
                                         "sigar_perfstat_cpu_total");

    if (!sigar->perfstat.cpu_total) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "dlsym(sigar_perfstat_cpu_total) failed: %s",
                             dlerror());
        }

        dlclose(handle);

        sigar->perfstat.avail = -1;
        return ENOENT;
    }

    sigar->perfstat.cpu =
        (perfstat_cpu_func_t)dlsym(handle,
                                   "sigar_perfstat_cpu");

    if (!sigar->perfstat.cpu) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "dlsym(sigar_perfstat_cpu) failed: %s",
                             dlerror());
        }

        dlclose(handle);

        sigar->perfstat.avail = -1;
        return ENOENT;
    }

    sigar->perfstat.swap =
        (perfstat_swap_func_t)dlsym(handle,
                                    "sigar_perfstat_pagingspace");

    if (!sigar->perfstat.swap) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "dlsym(sigar_perfstat_pagingspace) failed: %s",
                             dlerror());
        }

        dlclose(handle);

        sigar->perfstat.avail = -1;
        return ENOENT;
    }

    sigar->perfstat.mem =
        (perfstat_mem_func_t)dlsym(handle,
                                   "sigar_perfstat_memory");

    if (!sigar->perfstat.mem) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "dlsym(sigar_perfstat_memory) failed: %s",
                             dlerror());
        }

        dlclose(handle);

        sigar->perfstat.avail = -1;
        return ENOENT;
    }

    sigar->perfstat.disk =
        (perfstat_disk_func_t)dlsym(handle,
                                    "sigar_perfstat_disk");

    if (!sigar->perfstat.disk) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "dlsym(sigar_perfstat_disk) failed: %s",
                             dlerror());
        }

        dlclose(handle);

        sigar->perfstat.avail = -1;
        return ENOENT;
    }

    sigar->perfstat.ifstat =
        (perfstat_ifstat_func_t)dlsym(handle,
                                      "sigar_perfstat_netinterface");

    if (!sigar->perfstat.ifstat) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "dlsym(sigar_perfstat_netinterface) failed: %s",
                             dlerror());
        }

        dlclose(handle);

        sigar->perfstat.avail = -1;
        return ENOENT;
    }

    sigar->perfstat.protocol =
        (perfstat_protocol_func_t)dlsym(handle,
                                        "sigar_perfstat_protocol");

    if (!sigar->perfstat.protocol) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "dlsym(sigar_perfstat_protocol) failed: %s",
                             dlerror());
        }

        dlclose(handle);

        sigar->perfstat.avail = -1;
        return ENOENT;
    }

    sigar->perfstat.avail = 1;
    sigar->perfstat.handle = handle;

    return SIGAR_OK;
}

#define PAGESHIFT(v) \
    ((v) << sigar->pagesize)

int sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
    int status;

    if (sigar_perfstat_init(sigar) == SIGAR_OK) {
        perfstat_memory_total_t minfo;

        sigar_log(sigar, SIGAR_LOG_DEBUG, "[mem] using libperfstat");

        if (sigar->perfstat.mem(&minfo) == 1) {
            mem->total = PAGESHIFT(minfo.real_total);
            mem->free  = PAGESHIFT(minfo.real_free);
        }
        else {
            return errno;
        }            
    }
    else {
        struct vminfo vm;

        sigar_log(sigar, SIGAR_LOG_DEBUG, "[mem] using /dev/kmem");

        status = kread(sigar, &vm, sizeof(vm),
                       sigar->koffsets[KOFFSET_VMINFO]);

        if (status != SIGAR_OK) {
            return status;
        }

        mem->total = PAGESHIFT(vm.memsizepgs); /* lsattr -El sys0 -a realmem */
        mem->free  = PAGESHIFT(vm.numfrb);
    }

    mem->used = mem->total - mem->free;
    mem->actual_used = mem->used;
    mem->actual_free = mem->free;
    
    sigar_mem_calc_ram(sigar, mem);

    return SIGAR_OK;
}

static void swaps_free(swaps_t *swaps)
{
    if (swaps->num) {
        int i;

        for (i=0; i<swaps->num; i++) {
            free(swaps->devs[i]);
        }

        free(swaps->devs);

        swaps->num = 0;
    }
}

/*
 * there is no public api for parsing this file.
 * well, there is something, but its super ugly and requires
 * linking 2 static libraries (libodm and something else)
 * maybe will switch to that if it can add value elsewhere too.
 */
#define SWAPSPACES "/etc/swapspaces"

static int swaps_get(swaps_t *swaps)
{
    FILE *fp;
    char buf[512];
    char *ptr;
    struct stat statbuf;

    if (stat(SWAPSPACES, &statbuf) < 0) {
        return errno;
    }

    /* only re-parse if file has changed */
    if (swaps->mtime == statbuf.st_mtime) {
        return 0;
    }

    swaps->mtime = statbuf.st_mtime;

    /* easier to just start from scratch */
    swaps_free(swaps);

    if (!(fp = fopen(SWAPSPACES, "r"))) {
        return errno;
    }

    while ((ptr = fgets(buf, sizeof(buf), fp))) {
        if (!isalpha(*ptr)) {
            continue;
        }

        if (strchr(ptr, ':')) {
            int len;

            ptr = fgets(buf, sizeof(buf), fp);

            while (isspace(*ptr)) {
                ++ptr;
            }

            if (strncmp(ptr, "dev", 3)) {
                continue;
            }
            ptr += 3;
            while (isspace(*ptr) || (*ptr == '=')) {
                ++ptr;
            }

            len = strlen(ptr);
            ptr[len-1] = '\0'; /* -1 == chomp \n */

            swaps->devs = realloc(swaps->devs, swaps->num+1 * sizeof(char *));
            swaps->devs[swaps->num] = malloc(len);
            memcpy(swaps->devs[swaps->num], ptr, len);

            swaps->num++;
        }
    }

    fclose(fp);

    return 0;
}

/* 
 * documented in aix tech ref,
 * but this prototype is not in any friggin header file.
 * struct pginfo is in sys/vminfo.h
 */

int swapqry(char *path, struct pginfo *info);

static int sigar_swap_get_swapqry(sigar_t *sigar, sigar_swap_t *swap)
{
    int status, i;

    if ((status = swaps_get(&sigar->swaps)) != SIGAR_OK) {
        return status;
    }

    if (SIGAR_LOG_IS_DEBUG(sigar)) {
        sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                         "[swap] pagesize=%d, shift=%d",
                         getpagesize(), sigar->pagesize);
    }

    swap->total = swap->free = 0;

    for (i=0; i<sigar->swaps.num; i++) {
        struct pginfo info;

        status = swapqry(sigar->swaps.devs[i], &info);

        if (status != 0) {
            if (SIGAR_LOG_IS_DEBUG(sigar)) {
                sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                                 "[swap] swapqry(%s) failed: %s",
                                 sigar->swaps.devs[i],
                                 sigar_strerror(sigar, errno));
            }
            continue;
        }

        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "[swap] %s total=%d/%d, free=%d/%d",
                             sigar->swaps.devs[i],
                             info.size, PAGESHIFT(info.size),
                             info.free, PAGESHIFT(info.free));
        }

        swap->total += PAGESHIFT(info.size); /* lsps -a */
        swap->free  += PAGESHIFT(info.free);
    }

    swap->used = swap->total - swap->free;

    return SIGAR_OK;
}

#define SWAP_DEV(ps) \
   ((ps.type == LV_PAGING) ? \
     ps.id.lv_paging.vgname : \
     ps.id.nfs_paging.filename)

#define SWAP_MB_TO_BYTES(v) ((v) * (1024 * 1024))

static int sigar_swap_get_perfstat(sigar_t *sigar, sigar_swap_t *swap)
{
    perfstat_memory_total_t minfo;
    perfstat_pagingspace_t ps;
    perfstat_id_t id;

    id.name[0] = '\0';

    SIGAR_ZERO(swap);

    do {
        if (sigar->perfstat.swap(&id, &ps, 1) != 1) {
            if (SIGAR_LOG_IS_DEBUG(sigar)) {
                sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                                 "[swap] dev=%s query failed: %s",
                                 SWAP_DEV(ps),
                                 sigar_strerror(sigar, errno));
            }
            continue;
        }
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "[swap] dev=%s: active=%s, "
                             "total=%lluMb, used=%lluMb",
                             SWAP_DEV(ps),
                             ((ps.active == 1) ? "yes" : "no"),
                             ps.mb_size, ps.mb_used);
        }
        if (ps.active != 1) {
            continue;
        }
        /* convert MB sizes to bytes */
        swap->total += SWAP_MB_TO_BYTES(ps.mb_size);
        swap->used  += SWAP_MB_TO_BYTES(ps.mb_used);
    } while (id.name[0] != '\0');

    swap->free = swap->total - swap->used;

    if (sigar->perfstat.mem(&minfo) == 1) {
        swap->page_in = minfo.pgins;
        swap->page_out = minfo.pgouts;
    }
            
    return SIGAR_OK;
}

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
    swap->page_in = swap->page_out = -1;

    if (sigar_perfstat_init(sigar) == SIGAR_OK) {
        sigar_log(sigar, SIGAR_LOG_DEBUG, "[swap] using libperfstat");
        return sigar_swap_get_perfstat(sigar, swap);
    }
    else {
        sigar_log(sigar, SIGAR_LOG_DEBUG, "[swap] using /dev/kmem");
        return sigar_swap_get_swapqry(sigar, swap);
    }
}

int sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
    int i, status;
    struct sysinfo data;
    perfstat_cpu_total_t cpu_data;

    if (sigar_perfstat_init(sigar) == SIGAR_OK) {
        sigar_log(sigar, SIGAR_LOG_DEBUG, "[cpu] using libperfstat");

        if (sigar->perfstat.cpu_total(&cpu_data) == 1) {
            cpu->user  = SIGAR_TICK2MSEC(cpu_data.user);
            cpu->nice  = SIGAR_FIELD_NOTIMPL; /* N/A */
            cpu->sys   = SIGAR_TICK2MSEC(cpu_data.sys);
            cpu->idle  = SIGAR_TICK2MSEC(cpu_data.idle);
            cpu->wait  = SIGAR_TICK2MSEC(cpu_data.wait);
            cpu->total = cpu->user + cpu->sys + cpu->idle + cpu->wait;
            return SIGAR_OK;
        }
    }

    sigar_log(sigar, SIGAR_LOG_DEBUG, "[cpu] using /dev/kmem");

    status = kread(sigar, &data, sizeof(data),
                   sigar->koffsets[KOFFSET_SYSINFO]);

    if (status != SIGAR_OK) {
        return status;
    }

    cpu->user = SIGAR_TICK2MSEC(data.cpu[CPU_USER]);
    cpu->nice = SIGAR_FIELD_NOTIMPL; /* N/A */
    cpu->sys  = SIGAR_TICK2MSEC(data.cpu[CPU_KERNEL]);
    cpu->idle = SIGAR_TICK2MSEC(data.cpu[CPU_IDLE]);
    cpu->wait = SIGAR_TICK2MSEC(data.cpu[CPU_WAIT]);
    cpu->total = cpu->user + cpu->sys + cpu->idle + cpu->wait;

    return SIGAR_OK;
}

/*
 * other possible metrics we could add:
 * struct cpuinfo {
 *       long    cpu[CPU_NTIMES];
 *       long    pswitch;
 *       long    syscall;
 *       long    sysread;
 *       long    syswrite;
 *       long    sysfork;
 *       long    sysexec;
 *       long    readch;
 *       long    writech;
 *       long    iget;
 *       long    namei;
 *       long    dirblk;
 *       long    msg;
 *       long    sema;
 *       long    bread;
 *       long    bwrite;
 *       long    lread;
 *       long    lwrite;
 *       long    phread;
 *       long    phwrite;
 * };
 */

static int sigar_cpu_list_get_kmem(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    int status, i, j;
    int ncpu = _system_configuration.ncpus; /* this can change */
    int size = ncpu * sizeof(struct cpuinfo);

    if (sigar->cpuinfo_size < size) {
        sigar->cpuinfo = realloc(sigar->cpuinfo, size);
    }

    status = kread(sigar, sigar->cpuinfo, size,
                   sigar->koffsets[KOFFSET_CPUINFO]);

    if (status != SIGAR_OK) {
        return status;
    }

    sigar_cpu_list_create(cpulist);

    for (i=0; i<ncpu; i++) {
        sigar_cpu_t *cpu;
        struct cpuinfo *info;

        SIGAR_CPU_LIST_GROW(cpulist);

        cpu = &cpulist->data[cpulist->number++];

        info = &sigar->cpuinfo[i];
        cpu->user = SIGAR_TICK2MSEC(info->cpu[CPU_USER]);
        cpu->nice = 0; /* N/A */
        cpu->sys  = SIGAR_TICK2MSEC(info->cpu[CPU_KERNEL]);
        cpu->idle = SIGAR_TICK2MSEC(info->cpu[CPU_IDLE]);
        cpu->wait = SIGAR_TICK2MSEC(info->cpu[CPU_WAIT]);
        cpu->total = cpu->user + cpu->sys + cpu->idle + cpu->wait;
    }

    return SIGAR_OK;
}

static int sigar_cpu_list_get_pstat(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    perfstat_cpu_t data;
    int i, ncpu = _system_configuration.ncpus; /* this can change */
    perfstat_id_t id;

    id.name[0] = '\0';

    sigar_cpu_list_create(cpulist);

    for (i=0; i<ncpu; i++) {
        sigar_cpu_t *cpu;

        SIGAR_CPU_LIST_GROW(cpulist);

        cpu = &cpulist->data[cpulist->number++];

        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "cpu%d perfstat_id='%s'",
                             i, id.name);
        }

        if (sigar->perfstat.cpu(&id, &data, 1) == 1) {
            cpu->user  = SIGAR_TICK2MSEC(data.user);
            cpu->nice  = SIGAR_FIELD_NOTIMPL; /* N/A */
            cpu->sys   = SIGAR_TICK2MSEC(data.sys);
            cpu->idle  = SIGAR_TICK2MSEC(data.idle);
            cpu->wait  = SIGAR_TICK2MSEC(data.wait);
            cpu->total = cpu->user + cpu->sys + cpu->idle + cpu->wait;
        }
        else {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                             "cpu%d perfstat_cpu(%s) failed: %s",
                             i, id.name, sigar_strerror(sigar, errno));
            SIGAR_ZERO(cpu);
        }
    }

    return SIGAR_OK;
}

int sigar_cpu_list_get(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    if (sigar_perfstat_init(sigar) == SIGAR_OK) {
        sigar_log(sigar, SIGAR_LOG_DEBUG, "[cpu_list] using libperfstat");
        return sigar_cpu_list_get_pstat(sigar, cpulist);
    }
    else {
        sigar_log(sigar, SIGAR_LOG_DEBUG, "[cpu_list] using /dev/kmem");
        return sigar_cpu_list_get_kmem(sigar, cpulist);
    }
}

static int boot_time(sigar_t *sigar, time_t *time)
{
    int fd;
    struct utmp data;

    if ((fd = open(UTMP_FILE, O_RDONLY)) < 0) {
        return errno;
    }

    do {
        if (read(fd, &data, sizeof(data)) != sizeof(data)) {
            int status = errno;
            close(fd);
            return status;
        }
    } while (data.ut_type != BOOT_TIME);

    *time = data.ut_time;

    close(fd);

    return SIGAR_OK;
}

int sigar_uptime_get(sigar_t *sigar,
                     sigar_uptime_t *uptime)
{
    if (sigar->boot_time == 0) {
        int status;
        time_t time;

        if ((status = boot_time(sigar, &time)) != SIGAR_OK) {
            return status;
        }

        sigar->boot_time = time;
    }

    uptime->uptime = time(NULL) - sigar->boot_time;

    return SIGAR_OK;
}

#define WHOCPY(dest, src) \
    SIGAR_SSTRCPY(dest, src); \
    if (sizeof(src) < sizeof(dest)) \
        dest[sizeof(dest)-1] = '\0'

static int sigar_who_utmp(sigar_t *sigar,
                          sigar_who_list_t *wholist)
{
    struct utmp ut;
    FILE *fp;

    if (!(fp = fopen(UTMP_FILE, "r"))) {
        return errno;
    }

    while (fread(&ut, sizeof(ut), 1, fp) == 1) {
        sigar_who_t *who;

        if (*ut.ut_name == '\0') {
            continue;
        }

        if (ut.ut_type != USER_PROCESS) {
            continue;
        }

        SIGAR_WHO_LIST_GROW(wholist);
        who = &wholist->data[wholist->number++];

        WHOCPY(who->user, ut.ut_user);
        WHOCPY(who->device, ut.ut_line);
        WHOCPY(who->host, ut.ut_host);

        who->time = ut.ut_time;
    }

    fclose(fp);

    return SIGAR_OK;
}

int sigar_who_list_get(sigar_t *sigar,
                       sigar_who_list_t *wholist)
{
    int status;

    sigar_who_list_create(wholist);

    status = sigar_who_utmp(sigar, wholist);
    if (status != SIGAR_OK) {
        sigar_who_list_destroy(sigar, wholist);
        return status;
    }

    return SIGAR_OK;
}

int sigar_loadavg_get(sigar_t *sigar,
                      sigar_loadavg_t *loadavg)
{
    int status, i;
    int data[3];
    perfstat_cpu_total_t cpu_data;

    if (sigar_perfstat_init(sigar) == SIGAR_OK) {
        sigar_log(sigar, SIGAR_LOG_DEBUG,
                  "[loadavg] using libperfstat");

        if (sigar->perfstat.cpu_total(&cpu_data) == 1) {
            for (i=0; i<3; i++) {
                loadavg->loadavg[i] = FIXED_TO_DOUBLE(cpu_data.loadavg[i]);
            }
            return SIGAR_OK;
        }
        else {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                             "perfstat_cpu_total failed: %s",
                             sigar_strerror(sigar, errno));
        }
    }

    sigar_log(sigar, SIGAR_LOG_DEBUG,
              "[loadavg] using /dev/kmem");

    status = kread(sigar, &data, sizeof(data),
                   sigar->koffsets[KOFFSET_LOADAVG]);

    if (status != SIGAR_OK) {
        return status;
    }

    for (i=0; i<3; i++) {
        loadavg->loadavg[i] = FIXED_TO_DOUBLE(data[i]);
    }

    return SIGAR_OK;
}

int sigar_os_proc_list_get(sigar_t *sigar,
                           sigar_proc_list_t *proclist)
{
    pid_t pid = 0;
    struct procsinfo info;

    for (;;) {
        int num = getprocs(&info, sizeof(info),
                           NULL, 0, &pid, 1);

        if (num == 0) {
            break;
        }

        SIGAR_PROC_LIST_GROW(proclist);

        proclist->data[proclist->number++] = info.pi_pid;
    }

    return SIGAR_OK;
}

static int sigar_getprocs(sigar_t *sigar, sigar_pid_t pid)
{
    int status, num;
    time_t timenow = time(NULL);

    if (sigar->pinfo == NULL) {
        sigar->pinfo = malloc(sizeof(*sigar->pinfo));
    }

    if (sigar->last_pid == pid) {
        if ((timenow - sigar->last_getprocs) < SIGAR_LAST_PROC_EXPIRE) {
            return SIGAR_OK;
        }
    }

    sigar->last_pid = pid;
    sigar->last_getprocs = timenow;

    num = getprocs(sigar->pinfo, sizeof(*sigar->pinfo),
                   NULL, 0, &pid, 1);

    if (num != 1) {
        return ESRCH;
    }

    return SIGAR_OK;
}

int sigar_proc_mem_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_mem_t *procmem)
{
    int status = sigar_getprocs(sigar, pid);
    struct procsinfo64 *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    procmem->size  = PAGESHIFT(pinfo->pi_size); /* XXX fold in pi_dvm ? */
    procmem->share = PAGESHIFT(pinfo->pi_sdsize);
    procmem->resident = PAGESHIFT(pinfo->pi_drss + pinfo->pi_trss);

    procmem->minor_faults = pinfo->pi_minflt;
    procmem->major_faults = pinfo->pi_majflt;
    procmem->page_faults =
        procmem->minor_faults +
        procmem->major_faults;

    return SIGAR_OK;
}

int sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_cred_t *proccred)
{
    int status = sigar_getprocs(sigar, pid);
    struct procsinfo64 *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    proccred->uid  = pinfo->pi_cred.cr_ruid;
    proccred->euid = pinfo->pi_cred.cr_uid;
    if (proccred->uid == -1) {
        /* 
         * aix 5.2 has a process named 'jfsz'
         * where uid is '-1', getpwuid returns EPERM
         */
        proccred->uid = proccred->euid = 0;
    }
    proccred->gid  = pinfo->pi_cred.cr_rgid;
    proccred->egid = pinfo->pi_cred.cr_gid;

    return SIGAR_OK;
}

int sigar_proc_time_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_time_t *proctime)
{
    int status = sigar_getprocs(sigar, pid);
    struct procsinfo64 *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    proctime->start_time = pinfo->pi_start;
    proctime->start_time *= SIGAR_MSEC; /* convert to ms */
    proctime->user = pinfo->pi_utime * SIGAR_MSEC;
    proctime->sys  = pinfo->pi_stime * SIGAR_MSEC;
    proctime->total = proctime->user + proctime->sys;

    return SIGAR_OK;
}

int sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_state_t *procstate)
{
    int status = sigar_getprocs(sigar, pid);
    struct procsinfo64 *pinfo = sigar->pinfo;
    tid_t tid = 0;
    struct thrdsinfo64 thrinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    if (getthrds(pid, &thrinfo, sizeof(thrinfo), &tid, 1) == 1) {
        procstate->processor = thrinfo.ti_affinity;
    }
    else {
        procstate->processor = SIGAR_FIELD_NOTIMPL;
    }
    
    SIGAR_SSTRCPY(procstate->name, pinfo->pi_comm);
    procstate->ppid = pinfo->pi_ppid;
    procstate->nice = pinfo->pi_nice;
    procstate->tty  = pinfo->pi_ttyd;
    procstate->priority = pinfo->pi_pri;
    procstate->threads = pinfo->pi_thcount;

    switch (pinfo->pi_state) {
      case SACTIVE:
        procstate->state = 'R';
        break;
      case SIDL:
        procstate->state = 'D';
        break;
      case SSTOP:
        procstate->state = 'S';
        break;
      case SZOMB:
        procstate->state = 'Z';
        break;
      case SSWAP:
        procstate->state = 'S';
        break;
    }

    return SIGAR_OK;
}

int sigar_os_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                           sigar_proc_args_t *procargs)
{
    /* XXX if buffer is not large enough args are truncated */
    char buffer[8192], *ptr;
    struct procsinfo pinfo;

    pinfo.pi_pid = pid;

    if (getargs(&pinfo, sizeof(pinfo),
                buffer, sizeof(buffer)) != 0)
    {
        return errno;
    }

    ptr = buffer;

    while (*ptr) {
        int alen = strlen(ptr)+1;
        char *arg = malloc(alen);

        SIGAR_PROC_ARGS_GROW(procargs);
        memcpy(arg, ptr, alen);

        procargs->data[procargs->number++] = arg;
            
        ptr += alen;
    }

    return SIGAR_OK;
}

int sigar_proc_env_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_env_t *procenv)
{
    /* XXX if buffer is not large enough args are truncated */
    char buffer[8192], *ptr;
    struct procsinfo pinfo;

    pinfo.pi_pid = pid;

    if (getevars(&pinfo, sizeof(pinfo),
                 buffer, sizeof(buffer)) != 0)
    {
        return errno;
    }

    ptr = buffer;

    while (*ptr) {
        char *val = strchr(ptr, '=');
        int klen, vlen, status;
        char key[128]; /* XXX is there a max key size? */

        if (val == NULL) {
            /* not key=val format */
            procenv->env_getter(procenv->data, ptr, strlen(ptr), NULL, 0);
            break;
        }

        klen = val - ptr;
        SIGAR_SSTRCPY(key, ptr);
        key[klen] = '\0';
        ++val;

        vlen = strlen(val);
        status = procenv->env_getter(procenv->data,
                                     key, klen, val, vlen);

        if (status != SIGAR_OK) {
            /* not an error; just stop iterating */
            break;
        }

        ptr += (klen + 1 + vlen + 1);
    }

    return SIGAR_OK;
}

int sigar_proc_fd_get(sigar_t *sigar, sigar_pid_t pid,
                      sigar_proc_fd_t *procfd)
{
#ifdef SIGAR_64BIT
    /* XXX no getuser() in 64-bit mode */
    return SIGAR_ENOTIMPL;
#else
    int i;
    struct procsinfo pinfo;
    struct user uinfo;

    procfd->total = 0;
    pinfo.pi_pid = pid;

    if (getuser(&pinfo, sizeof(pinfo),
                &uinfo, sizeof(uinfo)) != 0) {
        if (errno == EINVAL) {
            return errno;
        }
    }

    /* see sys/user.h */
    for (i=0; i<uinfo.U_maxofile; i++) {
        if (uinfo.U_ofile(i)) {
            procfd->total++;
        }
    }

    return SIGAR_OK;
#endif
}

int sigar_proc_exe_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_exe_t *procexe)
{
    return SIGAR_ENOTIMPL;
}

static int sigar_proc_modules_local_get(sigar_t *sigar,
                                        sigar_proc_modules_t *procmods)
{
    struct ld_info *info;
    char *buffer;
    int size = 2048, status;
    unsigned int offset;

    buffer = malloc(size);
    while ((loadquery(L_GETINFO, buffer, size) == -1) &&
           (errno == ENOMEM))
    {
        size += 2048;
        buffer = realloc(buffer, size);
    }

    info = (struct ld_info *)buffer;

    do {
        char *name = info->ldinfo_filename;

        status = 
            procmods->module_getter(procmods->data, name, strlen(name));

        if (status != SIGAR_OK) {
            /* not an error; just stop iterating */
            free(buffer);
            return status;
        }
        
        offset = info->ldinfo_next;
        info = (struct ld_info *)((char*)info + offset);
    } while(offset);

    free(buffer);

    return SIGAR_OK;
}

int sigar_proc_modules_get(sigar_t *sigar, sigar_pid_t pid,
                           sigar_proc_modules_t *procmods)
{
    if (pid == sigar_pid_get(sigar)) {
        return sigar_proc_modules_local_get(sigar, procmods);
    }
    else {
        return SIGAR_ENOTIMPL;
    }
}

#define SIGAR_MICROSEC2NANO(s) \
    ((sigar_uint64_t)(s) * (sigar_uint64_t)1000)

#define TIME_NSEC(t) \
    (SIGAR_SEC2NANO((t).tv_sec) + SIGAR_MICROSEC2NANO((t).tv_usec))

int sigar_thread_cpu_get(sigar_t *sigar,
                         sigar_uint64_t id,
                         sigar_thread_cpu_t *cpu)
{
    struct rusage usage;
    int retval;

    sigar_perfstat_init(sigar);
    if (!sigar->perfstat.thread_rusage) {
        return SIGAR_ENOTIMPL;
    }

    if (sigar->thrusage != PTHRDSINFO_RUSAGE_START) {
        sigar->thrusage = PTHRDSINFO_RUSAGE_START;
        retval =
            sigar->perfstat.thread_rusage(&usage,
                                          PTHRDSINFO_RUSAGE_START);
        if (retval != 0) {
            return retval;
        }
    }

    retval = 
        sigar->perfstat.thread_rusage(&usage,
                                      PTHRDSINFO_RUSAGE_COLLECT);
    if (retval != 0) {
        return retval;
    }

    cpu->user  = TIME_NSEC(usage.ru_utime);
    cpu->sys   = TIME_NSEC(usage.ru_stime);
    cpu->total = TIME_NSEC(usage.ru_utime) + TIME_NSEC(usage.ru_stime);

    return SIGAR_OK;
}

int sigar_os_fs_type_get(sigar_file_system_t *fsp)
{
    return fsp->type;
}

#ifndef MNT_NFS4
/* another one documented in aix tech ref
 * with no friggin prototype in any header file...
 * ...but added in 5.2
 */
int mntctl(int command, int size, char *buffer);
#endif

int sigar_file_system_list_get(sigar_t *sigar,
                               sigar_file_system_list_t *fslist)
{
    int i, size, num;
    char *buf, *mntlist;

    /* get required size */
    if (mntctl(MCTL_QUERY, sizeof(size), (char *)&size) < 0) {
        return errno;
    }

    mntlist = buf = malloc(size);

    if ((num = mntctl(MCTL_QUERY, size, buf)) < 0) {
        free(buf);
        return errno;
    }

    sigar_file_system_list_create(fslist);

    for (i=0; i<num; i++) {
        char *devname;
        const char *typename = NULL;
        sigar_file_system_t *fsp;
        struct vmount *ent = (struct vmount *)mntlist;

        mntlist += ent->vmt_length;

        SIGAR_FILE_SYSTEM_LIST_GROW(fslist);

        fsp = &fslist->data[fslist->number++];

        switch (ent->vmt_gfstype) {
          case MNT_AIX:
            typename = "aix";
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
            break;
          case MNT_JFS:
            typename = "jfs";
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
            break;
          case MNT_NFS:
          case MNT_NFS3:
            typename = "nfs";
            fsp->type = SIGAR_FSTYPE_NETWORK;
            break;
          case MNT_CDROM:
            fsp->type = SIGAR_FSTYPE_CDROM;
            break;
          case MNT_SFS:
          case MNT_CACHEFS:
          case MNT_AUTOFS:
          default:
            if (ent->vmt_flags & MNT_REMOTE) {
                fsp->type = SIGAR_FSTYPE_NETWORK;
            }
            else {
                fsp->type = SIGAR_FSTYPE_NONE;
            }
        }

        SIGAR_SSTRCPY(fsp->dir_name, vmt2dataptr(ent, VMT_STUB));

        devname = vmt2dataptr(ent, VMT_OBJECT);

        if (fsp->type == SIGAR_FSTYPE_NETWORK) {
            char *hostname   = vmt2dataptr(ent, VMT_HOSTNAME);
#if 0
            /* XXX: these do not seem reliable */
            int hostname_len = vmt2datasize(ent, VMT_HOSTNAME)-1; /* -1 == skip '\0' */
            int devname_len  = vmt2datasize(ent, VMT_OBJECT);     /* includes '\0' */
#else
            int hostname_len = strlen(hostname);
            int devname_len = strlen(devname) + 1;
#endif
            int total_len    = hostname_len + devname_len + 1;    /* 1 == strlen(":") */

            if (total_len > sizeof(fsp->dev_name)) {
                /* justincase - prevent overflow.  chances: slim..none */
                SIGAR_SSTRCPY(fsp->dev_name, devname);
            }
            else {
                /* sprintf(fsp->devname, "%s:%s", hostname, devname) */
                char *ptr = fsp->dev_name;

                memcpy(ptr, hostname, hostname_len);
                ptr += hostname_len;

                *ptr++ = ':';

                memcpy(ptr, devname, devname_len);
            }
        }
        else {
            SIGAR_SSTRCPY(fsp->dev_name, devname);
        }

        /* we set fsp->type, just looking up sigar.c:fstype_names[type] */
        sigar_fs_type_get(fsp);

        if (typename == NULL) {
            typename = fsp->type_name;
        }

        SIGAR_SSTRCPY(fsp->sys_type_name, typename);
    }

    free(buf);

    return SIGAR_OK;
}

#define LSPV_CMD "/usr/sbin/lspv"

typedef struct {
    char name[IDENTIFIER_LENGTH];
    long addr;
} aix_diskio_t;

/*
 * dont have per-partition metrics on aix.
 * need to build a mount point => diskname map.
 * see 'lspv -l hdisk0' for example.
 */
static int create_diskmap_v4(sigar_t *sigar)
{
    FILE *fp = popen(LSPV_CMD, "r");
    char buffer[BUFSIZ], *ptr;

    sigar->diskmap = sigar_cache_new(25);

    if (!fp) {
        return ENOENT;
    }

    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        FILE *lfp;
        char cmd[256], disk[IDENTIFIER_LENGTH];
        char *s;

        if (strstr(ptr, " None")) {
            continue; /* no volume group */
        }
        if ((s = strchr(ptr, ' '))) {
            *s = '\0';
        }
        SIGAR_SSTRCPY(disk, ptr);
        snprintf(cmd, sizeof(cmd),
                 LSPV_CMD " -l %s", disk);
        if (!(lfp = popen(cmd, "r"))) {
            continue;
        }

        (void)fgets(buffer, sizeof(buffer), lfp); /* skip disk: */
        (void)fgets(buffer, sizeof(buffer), lfp); /* skip headers */
        while ((ptr = fgets(buffer, sizeof(buffer), lfp))) {
            sigar_cache_entry_t *ent;
            struct stat sb;
            int retval;
            /* LV NAME LPs PPs DISTRIBUTION */
            ptr = sigar_skip_multiple_token(ptr, 4);
            SIGAR_SKIP_SPACE(ptr);
            if ((s = strchr(ptr, '\n'))) {
                *s = '\0';
            }
            if (strEQ(ptr, "N/A")) {
                continue;
            }
            retval = stat(ptr, &sb);
            if (retval == 0) {
                aix_diskio_t *diskio = malloc(sizeof(*diskio));
                SIGAR_SSTRCPY(diskio->name, disk);
                diskio->addr = -1;
                ent = sigar_cache_get(sigar->diskmap, SIGAR_FSDEV_ID(sb));
                ent->value = diskio;
            }
        }
        pclose(lfp);
    }
    pclose(fp);
}

static int create_diskmap_v5(sigar_t *sigar)
{
    int i, total, num;
    perfstat_disk_t *disk;
    perfstat_id_t id;

    sigar_perfstat_init(sigar);
    if (!sigar->perfstat.disk) {
        return SIGAR_ENOTIMPL;
    }

    total = sigar->perfstat.disk(NULL, NULL, 0);
    if (total < 1) {
        return ENOENT;
    }

    disk = malloc(total * sizeof(*disk));
    id.name[0] = '\0';

    num = sigar->perfstat.disk(&id, disk, total);
    if (num < 1) {
        free(disk);
        return ENOENT;
    }

    sigar->diskmap = sigar_cache_new(25);

    odm_initialize();

    for (i=0; i<num; i++) {
        char query[256];
        struct CuDv *dv, *ptr;
        struct listinfo info;
        sigar_cache_entry_t *ent;
        int j;

        snprintf(query, sizeof(query),
                 "parent = '%s'", disk[i].vgname);

        ptr = dv = odm_get_list(CuDv_CLASS, query, &info, 256, 1);
        if ((int)dv == -1) {
            continue; /* XXX */
        }

        for (j=0; j<info.num; j++, ptr++) {
            struct CuAt *attr;
            int num, retval;
            struct stat sb;

            if ((attr = getattr(ptr->name, "label", 0, &num))) {
                retval = stat(attr->value, &sb);

                if (retval == 0) {
                    aix_diskio_t *diskio = malloc(sizeof(*diskio));
                    SIGAR_SSTRCPY(diskio->name, disk[i].name);
                    diskio->addr = -1;
                    ent = sigar_cache_get(sigar->diskmap, SIGAR_FSDEV_ID(sb));
                    ent->value = diskio;
                }

                free(attr);
            }
        }

        odm_free_list(dv, &info);
    }

    free(disk);
    odm_terminate();

    return SIGAR_OK;
}

static int create_diskmap(sigar_t *sigar)
{
    if (create_diskmap_v5(sigar) != SIGAR_OK) {
        return create_diskmap_v4(sigar);
    }
    return SIGAR_OK;
}

static void set_disk_metrics(struct dkstat *dkstat,
                             sigar_file_system_usage_t *fsusage)
{
    fsusage->disk.reads = dkstat->dk_rblks;
    fsusage->disk.writes = dkstat->dk_wblks;
    fsusage->disk.read_bytes  = dkstat->dk_rblks * dkstat->dk_bsize;
    fsusage->disk.write_bytes = dkstat->dk_wblks * dkstat->dk_bsize;
    fsusage->disk.time        = dkstat->dk_time;
    fsusage->disk.rtime       = SIGAR_FIELD_NOTIMPL;
    fsusage->disk.wtime       = SIGAR_FIELD_NOTIMPL;
    if (dkstat->dk_qd_magic == dk_q_depth_magic) {
        fsusage->disk.queue = dkstat->dk_q_depth;
    }
    else {
        fsusage->disk.queue = SIGAR_FIELD_NOTIMPL;
    }
}

static int get_disk_metrics(sigar_t *sigar,
                            sigar_file_system_usage_t *fsusage,
                            aix_diskio_t *diskio)
{
    int i, cnt, fd, status;
    struct iostat iostat;
    struct dkstat dkstat, *dp;
    struct nlist nl[] = {
        { "iostat" },
    };

    status = sigar_disk_usage_get(sigar, diskio->name, &fsusage->disk);
    if (status == SIGAR_OK) {
        return SIGAR_OK;
    }

    if (sigar->dmem == -1) {
        if ((sigar->dmem = open("/dev/mem", O_RDONLY)) <= 0) {
            return errno;
        }
    }

    fd = sigar->dmem;

    if (diskio->addr != -1) {
        int status;
        lseek(fd, diskio->addr, SEEK_SET);
        read(fd, &dkstat, sizeof(dkstat));

        if (strEQ(diskio->name, dkstat.diskname)) {
            set_disk_metrics(&dkstat, fsusage);
            status = SIGAR_OK;
        }
        else {
            status = ENOENT;
        }
        return status;
    }

    i = knlist(nl, 1, sizeof(struct nlist));

    if (i == -1) {
        return errno;
    }

    if (nl[i].n_value == 0) {
        return ENOENT;
    }

    lseek(fd, nl[0].n_value, SEEK_SET);
    read(fd, &iostat, sizeof(iostat));

    for (dp = iostat.dkstatp, cnt = iostat.dk_cnt;
         cnt && dp;
         --cnt, dp = dkstat.dknextp)
    {
        lseek(fd, (long)dp, SEEK_SET);
        read(fd, &dkstat, sizeof(dkstat));

        if (strEQ(diskio->name, dkstat.diskname)) {
            set_disk_metrics(&dkstat, fsusage);
            diskio->addr = (long)dp;
            break;
        }
    }

    return SIGAR_OK;
}

int sigar_disk_usage_get(sigar_t *sigar, const char *name,
                         sigar_disk_usage_t *usage)
{
    perfstat_disk_t disk;
    perfstat_id_t id;

    sigar_perfstat_init(sigar);
    if (!sigar->perfstat.disk) {
        return SIGAR_ENOTIMPL;
    }

    SIGAR_SSTRCPY(id.name, name);

    if (sigar->perfstat.disk(&id, &disk, 1) != 1) {
        return ENOENT;
    }

    usage->reads = disk.rblks;
    usage->writes = disk.wblks;
    usage->read_bytes  = disk.rblks * disk.bsize;
    usage->write_bytes = disk.wblks * disk.bsize;
    usage->queue       = disk.qdepth;
    usage->time        = disk.time;
    usage->rtime       = SIGAR_FIELD_NOTIMPL;
    usage->wtime       = SIGAR_FIELD_NOTIMPL;

    return SIGAR_OK;
}

/* XXX this is exactly the same as linux and hpux, solaris is darn close */

#define SIGAR_FS_BLOCKS_TO_BYTES(val, bsize) ((val * bsize) >> 1)

int sigar_file_system_usage_get(sigar_t *sigar,
                                const char *dirname,
                                sigar_file_system_usage_t *fsusage)
{
    sigar_cache_entry_t *ent;
    struct stat sb;
    int status;
    struct statfs buf;
    sigar_uint64_t val, bsize;
    
    if (statfs((char *)dirname, &buf) != 0) {
        return errno;
    }

    bsize = buf.f_bsize / 512;
    val = buf.f_blocks;
    fsusage->total = SIGAR_FS_BLOCKS_TO_BYTES(val, bsize);
    val = buf.f_bfree;
    fsusage->free  = SIGAR_FS_BLOCKS_TO_BYTES(val, bsize);
    val = buf.f_bavail;
    fsusage->avail = SIGAR_FS_BLOCKS_TO_BYTES(val, bsize);
    fsusage->used  = fsusage->total - fsusage->free;
    fsusage->files = buf.f_files;
    fsusage->free_files = buf.f_ffree;
    fsusage->use_percent = sigar_file_system_usage_calc_used(sigar, fsusage);

    SIGAR_DISK_STATS_INIT(&fsusage->disk);

    if (!sigar->diskmap) {
        status = create_diskmap(sigar);
        if (status != SIGAR_OK) {
            return SIGAR_OK;
        }
    }

    status = stat(dirname, &sb);
    if (status == 0) {
        sigar_cache_entry_t *ent =
            sigar_cache_get(sigar->diskmap, SIGAR_FSDEV_ID(sb));
        if (!ent->value) {
            return SIGAR_OK;
        }
        get_disk_metrics(sigar, fsusage, (aix_diskio_t *)ent->value);
    }

    return SIGAR_OK;
}

/* from sys/systemcfg.h, not defined in 4.3 headers */
#ifndef POWER_4
#define POWER_4		0x0800
#endif
#ifndef POWER_MPC7450
#define POWER_MPC7450	0x1000
#endif
#ifndef POWER_5
#define POWER_5		0x2000
#endif

static char *sigar_get_odm_model(sigar_t *sigar)
{
    if (sigar->model[0] == '\0') {
        struct CuAt *odm_obj;
        int num;

        odm_initialize();

        if ((odm_obj = getattr("proc0", "type", 0, &num))) {
            SIGAR_SSTRCPY(sigar->model, odm_obj->value);
            free(odm_obj);
        }

        odm_terminate();
    }

    return sigar->model;
}

#define SIGAR_CPU_CACHE_SIZE \
  (_system_configuration.L2_cache_size / 1024)

static int sigar_get_cpu_mhz_perfstat(sigar_t *sigar)
{
    perfstat_cpu_total_t data;

    if (sigar_perfstat_init(sigar) == SIGAR_OK) {
        if (sigar->perfstat.cpu_total(&data) == 1) {
            sigar->cpu_mhz = data.processorHZ / 1000000;
            return SIGAR_OK;
        }
        else {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                             "perfstat_cpu_total failed: %s",
                             sigar_strerror(sigar, errno));
        }
    }

    return ENOENT;
}

static int sigar_get_cpu_mhz(sigar_t *sigar)
{
    if (sigar->cpu_mhz == SIGAR_FIELD_NOTIMPL) {
        if (sigar_get_cpu_mhz_perfstat(sigar) != SIGAR_OK) {
            sigar_uint64_t cache_size = SIGAR_CPU_CACHE_SIZE;

            switch (cache_size) {
              case 1024:
                sigar->cpu_mhz = 333;
                break;
              case 4096:
                sigar->cpu_mhz = 400;
                break;
              case 8192:
                sigar->cpu_mhz = 450;
                break;
              default:
                sigar->cpu_mhz = SIGAR_FIELD_NOTIMPL;
                break;
            }
        }
    }

    return sigar->cpu_mhz;
}

static char *get_cpu_arch(void)
{
    switch (_system_configuration.architecture) {
        case POWER_RS:
            return "Power Classic";
        case POWER_PC:
            return "PowerPC";
        case IA64:
            return "IA64";
        default:
            return "PowerPC"; /* what else could it be */
    }
}

static char *get_ppc_cpu_model(void)
{
    switch (_system_configuration.implementation) {
        case POWER_RS1:
            return "RS1";
        case POWER_RSC:
            return "RSC";
        case POWER_RS2:
            return "RS2";
        case POWER_601:
            return "601";
        case POWER_603:
            return "603";
        case POWER_604:
            return "604";
        case POWER_620:
            return "620";
        case POWER_630:
            return "630";
        case POWER_A35:
            return "A35";
        case POWER_RS64II:
            return "RS64-II";
        case POWER_RS64III:
            return "RS64-III";
        case POWER_4:
            return "POWER4";
        case POWER_MPC7450:
            return "MPC7450";
        case POWER_5:
            return "POWER5";
        default:
            return "Unknown";
    }
}

static char *get_ia64_cpu_model(void)
{
    switch (_system_configuration.implementation) {
        case IA64_M1:
            return "M1";
        case IA64_M2:
            return "M2";
        default:
            return "Unknown";
    }
}

static char *get_cpu_model(void)
{
    if (_system_configuration.architecture == IA64) {
        return get_ia64_cpu_model();
    }
    else {
        return get_ppc_cpu_model();
    }
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    int i;
    int ncpu = _system_configuration.ncpus; /* this can change */
    char *arch = get_cpu_arch(), *model = get_cpu_model();

    /*XXX should only do this once*/
    sigar_cpu_info_list_create(cpu_infos);

    for (i=0; i<ncpu; i++) {
        sigar_cpu_info_t *info;

        SIGAR_CPU_INFO_LIST_GROW(cpu_infos);

        info = &cpu_infos->data[cpu_infos->number++];        

        info->cache_size = SIGAR_CPU_CACHE_SIZE;

        info->mhz = sigar_get_cpu_mhz(sigar);

        if (*arch == 'P') {
            SIGAR_SSTRCPY(info->vendor, "IBM");
        }
        else if (*arch == 'I') {
            SIGAR_SSTRCPY(info->vendor, "Intel");
        }
        else {
            SIGAR_SSTRCPY(info->vendor, "Unknown");
        }

        snprintf(info->model, sizeof(info->model),
                 "%s %s", arch, model);
    }

    return SIGAR_OK;
}

int sigar_net_route_list_get(sigar_t *sigar,
                             sigar_net_route_list_t *routelist)
{
    return SIGAR_ENOTIMPL;
}

static int sigar_net_interface_stat_get_kmem(sigar_t *sigar,
                                             const char *name,
                                             sigar_net_interface_stat_t *ifstat)
{
    int status;
    struct ifnet data;
    caddr_t offset = 0;
    char if_name[32];

    sigar_log(sigar, SIGAR_LOG_DEBUG, "[ifstat] using /dev/kmem");

    status = kread(sigar, &offset, sizeof(offset),
                   sigar->koffsets[KOFFSET_IFNET]);

    if (status != SIGAR_OK) {
        return status;
    }

    for (; offset; offset = (caddr_t)data.if_next) {
        status = kread(sigar, &data, sizeof(data), (long)offset);

        if (status != SIGAR_OK) {
            return status;
        }

        status = kread(sigar, if_name, sizeof(if_name),
                       (long)&data.if_name[0]);

        if (status != SIGAR_OK) {
            return status;
        }

        /* XXX if_name is 'en' or 'lo', not 'en0' or 'lo0' */
        if (!strnEQ(if_name, name, strlen(if_name))) {
            continue;
        }

        ifstat->rx_bytes      = data.if_ibytes;
        ifstat->rx_packets    = data.if_ipackets;
        ifstat->rx_errors     = data.if_ierrors;
        ifstat->rx_dropped    = data.if_iqdrops;
        ifstat->rx_overruns   = SIGAR_FIELD_NOTIMPL;
        ifstat->rx_frame      = SIGAR_FIELD_NOTIMPL;

        ifstat->tx_bytes      = data.if_obytes;
        ifstat->tx_packets    = data.if_opackets;
        ifstat->tx_errors     = data.if_oerrors;
        ifstat->tx_dropped    = SIGAR_FIELD_NOTIMPL;
        ifstat->tx_overruns   = SIGAR_FIELD_NOTIMPL;
        ifstat->tx_collisions = data.if_collisions;
        ifstat->tx_carrier    = SIGAR_FIELD_NOTIMPL;

        ifstat->speed         = data.if_baudrate;

        return SIGAR_OK;
    }

    return ENXIO;
}

static int sigar_net_interface_stat_get_perfstat(sigar_t *sigar,
                                                 const char *name,
                                                 sigar_net_interface_stat_t *ifstat)
{
    perfstat_id_t id;
    perfstat_netinterface_t data;

    sigar_log(sigar, SIGAR_LOG_DEBUG, "[ifstat] using libperfstat");

    SIGAR_SSTRCPY(id.name, name);

    if (sigar->perfstat.ifstat(&id, &data) == 1) {
        ifstat->rx_bytes      = data.ibytes;
        ifstat->rx_packets    = data.ipackets;
        ifstat->rx_errors     = data.ierrors;
        ifstat->rx_dropped    = SIGAR_FIELD_NOTIMPL;
        ifstat->rx_overruns   = SIGAR_FIELD_NOTIMPL;
        ifstat->rx_frame      = SIGAR_FIELD_NOTIMPL;

        ifstat->tx_bytes      = data.obytes;
        ifstat->tx_packets    = data.opackets;
        ifstat->tx_errors     = data.oerrors;
        ifstat->tx_dropped    = SIGAR_FIELD_NOTIMPL;
        ifstat->tx_overruns   = SIGAR_FIELD_NOTIMPL;
        ifstat->tx_collisions = data.collisions;
        ifstat->tx_carrier    = SIGAR_FIELD_NOTIMPL;

        ifstat->speed         = data.bitrate;

        return SIGAR_OK;
    }

    if (SIGAR_LOG_IS_DEBUG(sigar)) {
        sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                         "[ifstat] dev=%s query failed: %s",
                         name,
                         sigar_strerror(sigar, errno));
    }

    return sigar_net_interface_stat_get_kmem(sigar, name, ifstat);
}

int sigar_net_interface_stat_get(sigar_t *sigar, const char *name,
                                 sigar_net_interface_stat_t *ifstat)
{
    sigar_perfstat_init(sigar);

    if (sigar->perfstat.ifstat) {
        return sigar_net_interface_stat_get_perfstat(sigar, name, ifstat);
    }
    else {
        return sigar_net_interface_stat_get_kmem(sigar, name, ifstat);
    }
}

#define IS_TCP_SERVER(state, flags) \
    ((flags & SIGAR_NETCONN_SERVER) && (state == TCPS_LISTEN))

#define IS_TCP_CLIENT(state, flags) \
    ((flags & SIGAR_NETCONN_CLIENT) && (state != TCPS_LISTEN))

static int net_conn_get_tcp(sigar_net_connection_walker_t *walker)
{
    sigar_t *sigar = walker->sigar;
    int flags = walker->flags;
    int status;
    struct inpcb tcp_inpcb;
    struct tcpcb tcpcb;
    struct inpcb *entry;

    status = kread(sigar, &tcp_inpcb, sizeof(tcp_inpcb),
                   sigar->koffsets[KOFFSET_TCB]);

    if (status != SIGAR_OK) {
        return status;
    }

    entry = tcp_inpcb.inp_next;
    while (entry) {
        struct inpcb pcb;
        int state;

        status = kread(sigar, &pcb, sizeof(pcb), (long)entry);
        if (status != SIGAR_OK) {
            return status;
        }
        status = kread(sigar, &tcpcb, sizeof(tcpcb), (long)pcb.inp_ppcb);
        if (status != SIGAR_OK) {
            return status;
        }

        state = tcpcb.t_state;
        if ((IS_TCP_SERVER(state, flags) ||
             IS_TCP_CLIENT(state, flags)))
        {
            sigar_net_connection_t conn;

            SIGAR_ZERO(&conn);

            conn.type = SIGAR_NETCONN_TCP;

            sigar_net_address_set(conn.local_address,
                                  pcb.inp_laddr.s_addr);

            sigar_net_address_set(conn.remote_address,
                                  pcb.inp_faddr.s_addr);

            conn.local_port  = ntohs(pcb.inp_lport);
            conn.remote_port = ntohs(pcb.inp_fport);

            conn.send_queue = conn.receive_queue = SIGAR_FIELD_NOTIMPL;

            switch (state) {
              case TCPS_CLOSED:
                conn.state = SIGAR_TCP_CLOSE;
                break;
              case TCPS_LISTEN:
                conn.state = SIGAR_TCP_LISTEN;
                break;
              case TCPS_SYN_SENT:
                conn.state = SIGAR_TCP_SYN_SENT;
                break;
              case TCPS_SYN_RECEIVED:
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

            if (walker->add_connection(walker, &conn) != SIGAR_OK) {
                break;
            }
        }

        entry = pcb.inp_next;
        if (entry == tcp_inpcb.inp_next) {
            break;
        }
    }

    return SIGAR_OK;
}

int sigar_net_connection_walk(sigar_net_connection_walker_t *walker)
{
    int status;

    if (walker->flags & SIGAR_NETCONN_TCP) {
        status = net_conn_get_tcp(walker);

        if (status != SIGAR_OK) {
            return status;
        }
    }
#if 0
    if (walker->flags & SIGAR_NETCONN_UDP) {
        status = net_conn_get_udp(walker);

        if (status != SIGAR_OK) {
            return status;
        }
    }
#endif
    return SIGAR_OK;
}

SIGAR_DECLARE(int)
sigar_tcp_get(sigar_t *sigar,
              sigar_tcp_t *tcp)
{
    perfstat_id_t id;
    perfstat_protocol_t proto;

    if (sigar_perfstat_init(sigar) != SIGAR_OK) {
        return SIGAR_ENOTIMPL;
    }

    SIGAR_SSTRCPY(id.name, "tcp");

    if (sigar->perfstat.protocol(&id, &proto, 1) != 1) {
        return ENOENT;
    }    

    tcp->active_opens = proto.u.tcp.initiated;
    tcp->passive_opens = proto.u.tcp.accepted;
    tcp->attempt_fails = proto.u.tcp.dropped;
    tcp->estab_resets = proto.u.tcp.dropped;
    tcp->curr_estab = proto.u.tcp.established;
    tcp->in_segs = proto.u.tcp.ipackets;
    tcp->out_segs = proto.u.tcp.opackets;
    tcp->retrans_segs = 0;
    tcp->in_errs = proto.u.tcp.ierrors;
    tcp->out_rsts = 0;
}

#define NFS_V2_STAT_SET(type) \
    nfs->null = proto.u.nfsv2.type.null; \
    nfs->getattr = proto.u.nfsv2.type.getattr; \
    nfs->setattr = proto.u.nfsv2.type.setattr; \
    nfs->root = proto.u.nfsv2.type.root; \
    nfs->lookup = proto.u.nfsv2.type.lookup; \
    nfs->readlink = proto.u.nfsv2.type.readlink; \
    nfs->read = proto.u.nfsv2.type.read; \
    nfs->writecache = proto.u.nfsv2.type.writecache; \
    nfs->write = proto.u.nfsv2.type.write; \
    nfs->create = proto.u.nfsv2.type.create; \
    nfs->remove = proto.u.nfsv2.type.remove; \
    nfs->rename = proto.u.nfsv2.type.rename; \
    nfs->link = proto.u.nfsv2.type.link; \
    nfs->symlink = proto.u.nfsv2.type.symlink; \
    nfs->mkdir = proto.u.nfsv2.type.mkdir; \
    nfs->rmdir = proto.u.nfsv2.type.rmdir; \
    nfs->readdir = proto.u.nfsv2.type.readdir; \
    nfs->fsstat = proto.u.nfsv2.type.statfs

int sigar_nfs_client_v2_get(sigar_t *sigar,
                            sigar_nfs_client_v2_t *nfs)
{
    perfstat_id_t id;
    perfstat_protocol_t proto;

    if (sigar_perfstat_init(sigar) != SIGAR_OK) {
        return SIGAR_ENOTIMPL;
    }

    SIGAR_SSTRCPY(id.name, "nfsv2");

    if (sigar->perfstat.protocol(&id, &proto, 1) != 1) {
        return ENOENT;
    }    

    NFS_V2_STAT_SET(client);

    return SIGAR_OK;
}

int sigar_nfs_server_v2_get(sigar_t *sigar,
                            sigar_nfs_server_v2_t *nfs)
{
    perfstat_id_t id;
    perfstat_protocol_t proto;

    if (sigar_perfstat_init(sigar) != SIGAR_OK) {
        return SIGAR_ENOTIMPL;
    }

    SIGAR_SSTRCPY(id.name, "nfsv2");

    if (sigar->perfstat.protocol(&id, &proto, 1) != 1) {
        return ENOENT;
    }    

    NFS_V2_STAT_SET(server);

    return SIGAR_OK;
}

#define NFS_V3_STAT_SET(type) \
    nfs->null = proto.u.nfsv3.type.null; \
    nfs->getattr = proto.u.nfsv3.type.getattr; \
    nfs->setattr = proto.u.nfsv3.type.setattr; \
    nfs->lookup = proto.u.nfsv3.type.lookup; \
    nfs->access = proto.u.nfsv3.type.access; \
    nfs->readlink = proto.u.nfsv3.type.readlink; \
    nfs->read = proto.u.nfsv3.type.read; \
    nfs->write = proto.u.nfsv3.type.write; \
    nfs->create = proto.u.nfsv3.type.create; \
    nfs->mkdir = proto.u.nfsv3.type.mkdir; \
    nfs->symlink = proto.u.nfsv3.type.symlink; \
    nfs->mknod = proto.u.nfsv3.type.mknod; \
    nfs->remove = proto.u.nfsv3.type.remove; \
    nfs->rmdir = proto.u.nfsv3.type.rmdir; \
    nfs->rename = proto.u.nfsv3.type.rename; \
    nfs->link = proto.u.nfsv3.type.link; \
    nfs->readdir = proto.u.nfsv3.type.readdir; \
    nfs->readdirplus = proto.u.nfsv3.type.readdirplus; \
    nfs->fsstat = proto.u.nfsv3.type.fsstat; \
    nfs->fsinfo = proto.u.nfsv3.type.fsinfo; \
    nfs->pathconf = proto.u.nfsv3.type.pathconf; \
    nfs->commit = proto.u.nfsv3.type.commit

int sigar_nfs_client_v3_get(sigar_t *sigar,
                            sigar_nfs_client_v3_t *nfs)
{
    perfstat_id_t id;
    perfstat_protocol_t proto;

    if (sigar_perfstat_init(sigar) != SIGAR_OK) {
        return SIGAR_ENOTIMPL;
    }

    SIGAR_SSTRCPY(id.name, "nfsv3");

    if (sigar->perfstat.protocol(&id, &proto, 1) != 1) {
        return ENOENT;
    }    

    NFS_V3_STAT_SET(client);

    return SIGAR_OK;
}

int sigar_nfs_server_v3_get(sigar_t *sigar,
                            sigar_nfs_server_v3_t *nfs)
{
    perfstat_id_t id;
    perfstat_protocol_t proto;

    if (sigar_perfstat_init(sigar) != SIGAR_OK) {
        return SIGAR_ENOTIMPL;
    }

    SIGAR_SSTRCPY(id.name, "nfsv3");

    if (sigar->perfstat.protocol(&id, &proto, 1) != 1) {
        return ENOENT;
    }    

    NFS_V3_STAT_SET(server);

    return SIGAR_OK;
}

/* derived from pidentd's k_aix432.c */
int sigar_proc_port_get(sigar_t *sigar, int protocol,
                        unsigned long port, sigar_pid_t *pidp)
{
    struct procsinfo pinfo;
    struct fdsinfo finfo;
    pid_t pid = 0;
    int type;
    
    switch (protocol) {
        case SIGAR_NETCONN_TCP:
            type = IPPROTO_TCP;
            break;
        case SIGAR_NETCONN_UDP:
            type = IPPROTO_UDP;
            break;
        default:
          return SIGAR_ENOTIMPL;
    }
    
    for (;;) {
        int fd, status;
        int num = getprocs(&pinfo, sizeof(pinfo),
                           &finfo, sizeof(finfo),
                           &pid, 1);

        if (num == 0) {
            break;
        }

        if ((pinfo.pi_state == 0) || (pinfo.pi_state == SZOMB)) {
            continue;
        }

        for (fd = 0; fd < pinfo.pi_maxofile; fd++) {
            struct file file;
            struct socket socket, *sockp;
            struct protosw protosw;
            struct domain domain;
            struct inpcb inpcb;
            long ptr;
            
            if (!(ptr = (long)finfo.pi_ufd[fd].fp)) {
                continue;
            }
            
            status = kread(sigar, &file, sizeof(file), ptr);
            if (status != SIGAR_OK) {
                continue;
            }

            if (file.f_type != DTYPE_SOCKET) {
                continue;
            }
            
            if (!(sockp = (struct socket *)file.f_data)) {
                continue;
            }
            
            status = kread(sigar, &socket, sizeof(socket), (long)sockp);
            if (status != SIGAR_OK) {
                continue;
            }

            if (!(ptr = (long)socket.so_proto)) {
                continue;
            }
            
            status = kread(sigar, &protosw, sizeof(protosw), ptr);
            if (status != SIGAR_OK) {
                continue;
            }

            if (protosw.pr_protocol != type) {
                continue;
            }
            
            if (!(ptr = (long)protosw.pr_domain)) {
                continue;
            }
            
            status = kread(sigar, &domain, sizeof(domain), ptr);
            if (status != SIGAR_OK) {
                continue;
            }

            if ((domain.dom_family != AF_INET) &&
                domain.dom_family != AF_INET6)
            {
                continue;
            }
            
            if (!(ptr = (long)socket.so_pcb)) {
                continue;
            }
            
            status = kread(sigar, &inpcb, sizeof(inpcb), ptr);
            if (status != SIGAR_OK) {
                continue;
            }

            if (sockp != inpcb.inp_socket) {
                continue;
            }

            if (inpcb.inp_lport != port) {
                continue;
            }
            
            *pidp = pinfo.pi_pid;

            return SIGAR_OK;
        }
    }

    return ENOENT;
}

int sigar_os_sys_info_get(sigar_t *sigar,
                          sigar_sys_info_t *sysinfo)
{
    struct utsname name;

    uname(&name);

    SIGAR_SSTRCPY(sysinfo->vendor, "IBM");
    SIGAR_SSTRCPY(sysinfo->arch, get_cpu_arch());
    /* utsname.machine is a sequence number */
    /* XXX odm might have something better */
    snprintf(sysinfo->machine,
             sizeof(sysinfo->machine),
             "%s %s",
             sysinfo->arch, get_cpu_model());

    snprintf(sysinfo->version,
             sizeof(sysinfo->version),
             "%s.%s",
             name.version, name.release);

    SIGAR_SSTRCPY(sysinfo->vendor_version, sysinfo->version);

    snprintf(sysinfo->description,
             sizeof(sysinfo->description),
             "%s %s",
             sysinfo->name, sysinfo->version);

    return SIGAR_OK;
}
