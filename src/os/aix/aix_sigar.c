#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"
#include "sigar_util.h"

#include <dlfcn.h>
#include <nlist.h>
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

/* for odm api */
#include <sys/cfgodm.h>
#include <sys/cfgdb.h>
#include <cf.h>

#include "user_v5.h"
#include "utmp_v5.h"

#include <sys/ldr.h>

/* not defined in aix 4.3 */
#ifndef SBITS
#define SBITS 16
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
    void *dlhandle;
    int kmem = -1;
    vminfo_func_t vminfo = NULL;
    struct utsname name;

    if ((dlhandle = dlopen("/unix", RTLD_NOW))) {
        vminfo = (vminfo_func_t)dlsym(dlhandle, "vmgetinfo");

        dlclose(dlhandle);
    }

    kmem = open("/dev/kmem", O_RDONLY);

    *sigar = malloc(sizeof(**sigar));

    (*sigar)->getvminfo = vminfo;
    (*sigar)->getprocfd = NULL; /*XXX*/
    (*sigar)->kmem = kmem;
    (*sigar)->pagesize = 0;
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

    (*sigar)->self_path[0] = '\0';

    uname(&name);

    (*sigar)->aix_version = atoi(name.version);

    return SIGAR_OK;
}

static void swaps_free(swaps_t *swaps);

int sigar_os_close(sigar_t *sigar)
{
    swaps_free(&sigar->swaps);
    if (sigar->kmem > 0) {
        close(sigar->kmem);
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
    free(sigar);
    return SIGAR_OK;
}

char *sigar_os_error_string(int err)
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

        SIGAR_SSTRCPY(sigar->self_path, name);

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
    if (sigar->self_path[0] == '\0') {
        sigar_proc_modules_t procmods;
        procmods.module_getter = proc_module_get_self;
        procmods.data = sigar;

        sigar_proc_modules_get(sigar, sigar_pid_get(sigar),
                               &procmods);

        if (sigar->self_path[0] == '\0') {
            /* dont try again */
            SIGAR_SSTRCPY(sigar->self_path, ".");
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
    char libperfstat[SIGAR_PATH_MAX], *path;
    int len;

    if (sigar->perfstat.avail == 1) {
        return SIGAR_OK;
    }
    if (sigar->perfstat.avail == -1) {
        return ENOENT;
    }

    path = sigar_get_self_path(sigar);
    len = strlen(path);

    memcpy(&path[0], sigar->self_path, len);
    memcpy(&path[len], SIGAR_AIXPERFSTAT, 
           sizeof(SIGAR_AIXPERFSTAT));

    if (!(handle = dlopen(path, RTLD_LOCAL|RTLD_LAZY))) {
        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "failed to open '%s': %s",
                             path, sigar_strerror(sigar, errno));
        }

        sigar->perfstat.avail = -1;
        return errno;
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

    sigar->perfstat.avail = 1;
    sigar->perfstat.handle = handle;

    return SIGAR_OK;
}

#define PAGESHIFT(v) \
    ((v) << sigar->pagesize)

int sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
    struct vminfo vm;

#if 0
    /* XXX: wtf, this is supposed to be a modern way
     * to get the same values below.  yet it works on 4.3.3
     * but not 5.1
     */
    if (!sigar->getvminfo) {
        return EPERM;
    }

    if (sigar->getvminfo(&vm, VMINFO, sizeof(vm)) != 0) {
        return errno;
    }

#else
    int status;

    status = kread(sigar, &vm, sizeof(vm),
                   sigar->koffsets[KOFFSET_VMINFO]);

    if (status != SIGAR_OK) {
        return status;
    }
#endif

    mem->total  = PAGESHIFT(vm.memsizepgs); /* lsattr -El sys0 -a realmem */
    mem->free   = PAGESHIFT(vm.numfrb);
    mem->used   = mem->total - mem->free;

    mem->shared = -1;
    mem->buffer = -1;
    mem->cached = -1;
    
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

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
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

int sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
    int i, status;
    struct sysinfo data;
    perfstat_cpu_total_t cpu_data;

    if (sigar_perfstat_init(sigar) == SIGAR_OK) {
        sigar_log(sigar, SIGAR_LOG_DEBUG, "[cpu] using libperfstat");

        if (sigar->perfstat.cpu_total(&cpu_data, sizeof(cpu_data))) {
            cpu->user  = cpu_data.user;
            cpu->nice  = -1; /* N/A */
            cpu->sys   = cpu_data.sys;
            cpu->idle  = cpu_data.idle;
            cpu->total = cpu->user + cpu->sys + cpu->idle + cpu_data.wait;
            return SIGAR_OK;
        }
    }

    sigar_log(sigar, SIGAR_LOG_DEBUG, "[cpu] using /dev/kmem");

    status = kread(sigar, &data, sizeof(data),
                   sigar->koffsets[KOFFSET_SYSINFO]);

    if (status != SIGAR_OK) {
        return status;
    }

    cpu->user = data.cpu[CPU_USER];
    cpu->nice = -1; /* N/A */
    cpu->sys  = data.cpu[CPU_KERNEL];
    cpu->idle = data.cpu[CPU_IDLE];
    cpu->total = 0;

    for (i=0; i<CPU_NTIMES; i++) {
        cpu->total += data.cpu[i];
    }

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
        cpu->user = info->cpu[CPU_USER];
        cpu->nice = 0; /* N/A */
        cpu->sys  = info->cpu[CPU_KERNEL];
        cpu->idle = info->cpu[CPU_IDLE];
        cpu->total = 0;

        for (j=0; j<CPU_NTIMES; j++) {
            cpu->total += info->cpu[j];
        }
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

        if (sigar->perfstat.cpu(&id, &data, sizeof(data), 1)) {
            cpu->user  = data.user;
            cpu->nice  = -1; /* N/A */
            cpu->sys   = data.sys;
            cpu->idle  = data.idle;
            cpu->total = cpu->user + cpu->sys + cpu->idle + data.wait;
        }
        else {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                             "cpu%d perfstat_cpu(%s) failed",
                             i, id.name);
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

static int boot_time_v4(int fd, time_t *time)
{
    struct utmp data;

    do {
        if (read(fd, &data, sizeof(data)) != sizeof(data)) {
            return errno;
        }
    } while (data.ut_type != BOOT_TIME);

    *time = data.ut_time;

    return SIGAR_OK;
}

static int boot_time_v5(int fd, time_t *time)
{
    struct utmp_v5 data;

    do {
        if (read(fd, &data, sizeof(data)) != sizeof(data)) {
            return errno;
        }
    } while (data.ut_type != BOOT_TIME);

    if (data.ut_time != 0) {
        *time = data.ut_time;
    }
    else {
        /* XXX: dunno wtf is going on here.
         * this exact code above works as expected
         * in a standalone program.
         * sizeof(utmp_v5) is the same if compiled on 4.3
         * or 5.2, this workaround hack will have todo for now.
         */
        *time = data.__time_t_space;
    }

    return SIGAR_OK;
}

static int boot_time(sigar_t *sigar, time_t *time)
{
    int utmp, status;

    if ((utmp = open(UTMP_FILE, O_RDONLY)) < 0) {
        return errno;
    }

    if (sigar->aix_version == 4) {
        status = boot_time_v4(utmp, time);
    }
    else {
        status = boot_time_v5(utmp, time);
    }

    close(utmp);

    return status;
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
    uptime->idletime = -1;

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

        if (sigar->perfstat.cpu_total(&cpu_data, sizeof(cpu_data))) {
            for (i=0; i<3; i++) {
                loadavg->loadavg[i] = FIXED_TO_DOUBLE(cpu_data.loadavg[i]);
            }
            return SIGAR_OK;
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

int sigar_proc_list_get(sigar_t *sigar,
                        sigar_proc_list_t *proclist)
{
    pid_t pid = 0;
    struct procsinfo info;

    sigar_proc_list_create(proclist);

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

int sigar_proc_stat_get(sigar_t *sigar,
                        sigar_proc_stat_t *procstat)
{
    int status = /* XXX optimize */
        sigar_proc_count(sigar, &procstat->total);

    return status;
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
    struct procsinfo *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    procmem->size  = PAGESHIFT(pinfo->pi_size);
    procmem->vsize = PAGESHIFT(pinfo->pi_dvm);
    procmem->share = PAGESHIFT(pinfo->pi_sdsize);
    procmem->rss   = PAGESHIFT(pinfo->pi_drss + pinfo->pi_trss);
    procmem->resident = -1; /* N/A */

    return SIGAR_OK;
}

int sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_cred_t *proccred)
{
    int status = sigar_getprocs(sigar, pid);
    struct procsinfo *pinfo = sigar->pinfo;

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
    struct procsinfo *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    proctime->start_time = pinfo->pi_start;
    proctime->start_time *= 1000; /* convert to ms */
    proctime->utime = pinfo->pi_utime;
    proctime->stime = pinfo->pi_stime;

    return SIGAR_OK;
}

int sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_state_t *procstate)
{
    int status = sigar_getprocs(sigar, pid);
    struct procsinfo *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    SIGAR_SSTRCPY(procstate->name, pinfo->pi_comm);
    procstate->ppid = pinfo->pi_ppid;
    procstate->nice = pinfo->pi_nice;
    procstate->tty  = pinfo->pi_ttyd;
    procstate->priority = -1; /* XXX getthrds() */
                
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

int sigar_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
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

    sigar_proc_args_create(procargs);

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

/*
 * V[45]_sigar_proc_fd_get routines are exactly
 * the same except for sizeof(uinfo).
 */
static int V5_sigar_proc_fd_get(sigar_t *sigar, sigar_pid_t pid,
                                sigar_proc_fd_t *procfd)
{
    int i;
    struct procsinfo pinfo;
    struct user_v5 uinfo; /* V5 */

    procfd->total = 0;
    pinfo.pi_pid = pid;

    if (getuser(&pinfo, sizeof(pinfo),
                &uinfo, sizeof(uinfo)) != 0) {
        if (errno == EINVAL) {
            return SIGAR_ENOTIMPL;
        }
        return errno;
    }

    /* see sys/user.h */
    for (i=0; i<uinfo.U_maxofile; i++) {
        if (uinfo.U_ufd[i].fp) {
            procfd->total++;
        }
    }

    return SIGAR_OK;
}

static int V4_sigar_proc_fd_get(sigar_t *sigar, sigar_pid_t pid,
                                sigar_proc_fd_t *procfd)
{
    int i;
    struct procsinfo pinfo;
    struct user uinfo; /* V4 */

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
        if (uinfo.U_ufd[i].fp) {
            procfd->total++;
        }
    }

    return SIGAR_OK;
}

int sigar_proc_fd_get(sigar_t *sigar, sigar_pid_t pid,
                      sigar_proc_fd_t *procfd)
{
    if (sigar->getprocfd == NULL) {
        /*
         * XXX should determine aix version in sigar_os_open
         * and set function pointer there.  for now try v4
         * first, if that fails try v5.  only costs 1 extra
         * call to getuser on v5 for the lifetime of the
         * sigar.
         */
        int status = V4_sigar_proc_fd_get(sigar, pid, procfd);

        if (status == SIGAR_OK) {
            sigar->getprocfd = V4_sigar_proc_fd_get;
            return SIGAR_OK;
        }

        sigar->getprocfd = V5_sigar_proc_fd_get;
    }

    return sigar->getprocfd(sigar, pid, procfd);
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

int sigar_os_fs_type_get(sigar_file_system_t *fsp)
{
    return fsp->type;
}

/* another one documented in aix tech ref
 * with no friggin prototype in any header file.
 */
int mntctl(int command, int size, char *buffer);

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

/* XXX this is exactly the same as linux and hpux, solaris is darn close */

#define SIGAR_FS_BLOCKS_TO_BYTES(buf, f) \
    ((buf.f * (buf.f_bsize / 512)) >> 1)

int sigar_file_system_usage_get(sigar_t *sigar,
                                const char *dirname,
                                sigar_file_system_usage_t *fsusage)
{
    struct statfs buf;

    if (statfs((char *)dirname, &buf) != 0) {
        return errno;
    }

    fsusage->total = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_blocks);
    fsusage->free  = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_bfree);
    fsusage->avail = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_bavail);
    fsusage->files = buf.f_files;
    fsusage->free_files = buf.f_ffree;
    fsusage->use_percent = sigar_file_system_usage_calc_used(sigar, fsusage);

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
        if (sigar->perfstat.cpu_total(&data, sizeof(data))) {
            sigar->cpu_mhz = data.processorHZ / 1000000;
            return SIGAR_OK;
        }
    }

    return ENOENT;
}

static int sigar_get_cpu_mhz(sigar_t *sigar)
{
    if (sigar->cpu_mhz == -1) {
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
                sigar->cpu_mhz = -1;
                break;
            }
        }
    }

    return sigar->cpu_mhz;
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    int i;
    int ncpu = _system_configuration.ncpus; /* this can change */

    /*XXX should only do this once*/
    sigar_cpu_info_list_create(cpu_infos);

    for (i=0; i<ncpu; i++) {
        sigar_cpu_info_t *info;
        char *arch, *model=NULL;

        SIGAR_CPU_INFO_LIST_GROW(cpu_infos);

        info = &cpu_infos->data[cpu_infos->number++];        

        info->cache_size = SIGAR_CPU_CACHE_SIZE;

        info->mhz = sigar_get_cpu_mhz(sigar);

        switch (_system_configuration.architecture) {
          case POWER_RS:
            arch = "Power Classic";
            break;
          case POWER_PC:
            arch = "PowerPC";
            break;
          case IA64:
            arch = "IA64";
            break;
          default:
            arch = "PowerPC"; /* what else could it be */
            break;
        }

        if (*arch == 'P') {
            SIGAR_SSTRCPY(info->vendor, "IBM");

            switch (_system_configuration.implementation) {
              case POWER_RS1:
                model = "RS1";
                break;
              case POWER_RSC:
                model = "RSC";
                break;
              case POWER_RS2:
                model = "RS2";
                break;
              case POWER_601:
                model = "601";
                break;
              case POWER_603:
                model = "603";
                break;
              case POWER_604:
                model = "604";
                break;
              case POWER_620:
                model = "620";
                break;
              case POWER_630:
                model = "630";
                break;
              case POWER_A35:
                model = "A35";
                break;
              case POWER_RS64II:
                model = "RS64-II";
                break;
              case POWER_RS64III:
                model = "RS64-III";
                break;
              case POWER_4:
                model = "POWER4";
                break;
              case POWER_MPC7450:
                model = "MPC7450";
                break;
              case POWER_5:
                model = "POWER5";
                break;
              default:
                break;
            }
        }
        else if (*arch == 'I') {
            SIGAR_SSTRCPY(info->vendor, "Intel");

            switch (_system_configuration.implementation) {
              case IA64_M1:
                model = "M1";
                break;
              case IA64_M2:
                model = "M2";
                break;
              default:
                break;
            }
        }
        else {
            SIGAR_SSTRCPY(info->vendor, "IBM");
            break;
        }

        if (model) {
            snprintf(info->model, sizeof(info->model),
                     "%s %s", arch, model);
        }
        else {
            SIGAR_SSTRCPY(info->model, arch);
        }
    }

    return SIGAR_OK;
}

int sigar_net_route_list_get(sigar_t *sigar,
                             sigar_net_route_list_t *routelist)
{
    sigar_net_route_t *route;

    sigar_net_route_list_create(routelist);

    return SIGAR_OK;
}

int sigar_net_interface_stat_get(sigar_t *sigar, const char *name,
                                 sigar_net_interface_stat_t *ifstat)
{
    int status;
    struct ifnet data;
    caddr_t offset = 0;
    char if_name[32];

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
        ifstat->rx_overruns   = -1;
        ifstat->rx_frame      = -1;

        ifstat->tx_bytes      = data.if_obytes;
        ifstat->tx_packets    = data.if_opackets;
        ifstat->tx_errors     = data.if_oerrors;
        ifstat->tx_dropped    = -1;
        ifstat->tx_overruns   = -1;
        ifstat->tx_collisions = data.if_collisions;
        ifstat->tx_carrier    = -1;

        return SIGAR_OK;
    }

    return ENXIO;
}

int sigar_net_connection_list_get(sigar_t *sigar,
                                  sigar_net_connection_list_t *connlist,
                                  int flags)
{
    return SIGAR_ENOTIMPL;
}
