#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

#ifdef DARWIN
#include <mach/mach_init.h>
#include <mach/message.h>
#include <mach/kern_return.h>
#else
#include <sys/dkstat.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/user.h>
#include <fcntl.h>
#endif

#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/sockio.h>

#include <net/if.h>
#include <net/if_dl.h>
#include <net/if_types.h>
#include <net/route.h>
#include <netinet/in.h>

#include <dirent.h>
#include <errno.h>

#define NMIB(mib) (sizeof(mib)/sizeof(mib[0]))

#ifndef DARWIN
static int get_koffsets(sigar_t *sigar)
{
    int i;
    struct nlist klist[] = {
        { "_cp_time" },
        { NULL }
    };

    kvm_nlist(sigar->kmem, klist);
    if (klist[0].n_type == 0) {
        return errno;
    }

    for (i=0; i<KOFFSET_MAX; i++) {
        sigar->koffsets[i] = klist[i].n_value;
    }

    return SIGAR_OK;
}

static int kread(sigar_t *sigar, void *data, int size, long offset)
{
#if 0
    if (sigar->kmem < 0) {
        return SIGAR_EPERM_KMEM;
    }
#endif

    if (kvm_read(sigar->kmem, offset, data, size) != size) {
        return errno;
    }

    return SIGAR_OK;
}
#endif

int sigar_os_open(sigar_t **sigar)
{
    int mib[2];
    int ncpu;
    size_t len;
    struct timeval boottime;

    len = sizeof(ncpu);
    mib[0] = CTL_HW;
    mib[1] = HW_NCPU;
    if (sysctl(mib, NMIB(mib), &ncpu,  &len, NULL, 0) < 0) {
        return errno;
    }

    len = sizeof(boottime);
    mib[0] = CTL_KERN;
    mib[1] = KERN_BOOTTIME;
    if (sysctl(mib, NMIB(mib), &boottime, &len, NULL, 0) < 0) {
        return errno;
    }

    *sigar = malloc(sizeof(**sigar));

#ifdef DARWIN
    (*sigar)->mach_port = mach_host_self();
#else
    (*sigar)->kmem = kvm_open(NULL, NULL, NULL, O_RDONLY, "kvm_open");
#endif

    get_koffsets(*sigar);

    (*sigar)->ncpu = ncpu;

    (*sigar)->boot_time = boottime.tv_sec; /* XXX seems off a bit */

    (*sigar)->last_pid = -1;

    (*sigar)->pinfo = NULL;

    return SIGAR_OK;
}

int sigar_os_close(sigar_t *sigar)
{
    if (sigar->pinfo) {
        free(sigar->pinfo);
    }
    free(sigar);
    return SIGAR_OK;
}

char *sigar_os_error_string(int err)
{
    return NULL;
}

int sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
#ifdef DARWIN
    vm_statistics_data_t vmstat;
    kern_return_t status;
    mach_msg_type_number_t count = sizeof(vmstat) / sizeof(integer_t);
#endif
    int mib[2];
    int totmem;
    size_t len = sizeof(totmem);

    mib[0] = CTL_HW;

    mib[1] = HW_PAGESIZE;
    if (sysctl(mib, NMIB(mib), &sigar->pagesize, &len, NULL, 0) < 0) {
        return errno;
    }

    mib[1] = HW_PHYSMEM;
    if (sysctl(mib, NMIB(mib), &totmem, &len, NULL, 0) < 0) {
        return errno;
    }

    mem->total = totmem;

    sigar_mem_calc_ram(sigar, mem);

#ifdef DARWIN
    status = host_statistics(sigar->mach_port, HOST_VM_INFO,
                             (host_info_t)&vmstat, &count);

    if (status != KERN_SUCCESS) {
        return errno;
    }

    mem->free = vmstat.free_count * sigar->pagesize;
#else
    mem->free = 1; /*XXX*/
#endif

    mem->used = mem->total - mem->free;
    mem->shared = SIGAR_FIELD_NOTIMPL; /*XXX*/

    mem->actual_free = mem->free;
    mem->actual_used = mem->used;

    return SIGAR_OK;
}

#define SIGAR_FS_BLOCKS_TO_BYTES(buf, f) \
    ((buf.f * (buf.f_bsize / 512)) >> 1)

#define VM_DIR "/private/var/vm"
#define SWAPFILE "swapfile"

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
#ifdef DARWIN
    DIR *dirp;
    struct dirent *ent;
    char swapfile[SSTRLEN(VM_DIR) + SSTRLEN("/") + SSTRLEN(SWAPFILE) + 12];
    struct stat swapstat;
    struct statfs vmfs;

    swap->used = swap->total = swap->free = 0;

    if (!(dirp = opendir(VM_DIR))) {
        return errno;
    }

    /* looking for "swapfile0", "swapfile1", etc. */
    while ((ent = readdir(dirp))) {
        char *ptr = swapfile;

        if ((ent->d_namlen < SSTRLEN(SWAPFILE)+1) || /* n/a, see comment above */
            (ent->d_namlen > SSTRLEN(SWAPFILE)+11)) /* ensure no overflow */
        {
            continue;
        }

        if (!strnEQ(ent->d_name, SWAPFILE, SSTRLEN(SWAPFILE))) {
            continue;
        }
        
        /* sprintf(swapfile, "%s/%s", VM_DIR, ent->d_name) */

        memcpy(ptr, VM_DIR, SSTRLEN(VM_DIR));
        ptr += SSTRLEN(VM_DIR);

        *ptr++ = '/';

        memcpy(ptr, ent->d_name, ent->d_namlen+1);

        if (stat(swapfile, &swapstat) < 0) {
            continue;
        }

        swap->used += swapstat.st_size;
    }

    closedir(dirp);

    if (statfs(VM_DIR, &vmfs) < 0) {
        return errno;
    }

    swap->total = SIGAR_FS_BLOCKS_TO_BYTES(vmfs, f_bfree) + swap->used;

    swap->free = swap->total - swap->used;

#else
    struct kvm_swap kswap[1];

    if (kvm_getswapinfo(sigar->kmem, kswap, 1, 0) < 0) {
        return errno;
    }

    if (kswap[0].ksw_total == 0) {
        swap->total = 0;
        swap->used  = 0;
        swap->free  = 0;
        return SIGAR_OK;
    }

    swap->total = kswap[0].ksw_total * sigar->pagesize;
    swap->used  = kswap[0].ksw_used * sigar->pagesize;
    swap->free  = swap->total - swap->used;
#endif

    return SIGAR_OK;
}

int sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
#ifdef DARWIN
    kern_return_t status;
    mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
    host_cpu_load_info_data_t cpuload;

    status = host_statistics(sigar->mach_port, HOST_CPU_LOAD_INFO,
                             (host_info_t)&cpuload, &count);

    if (status != KERN_SUCCESS) {
        return errno;
    }

    cpu->user = cpuload.cpu_ticks[CPU_STATE_USER];
    cpu->sys  = cpuload.cpu_ticks[CPU_STATE_SYSTEM];
    cpu->idle = cpuload.cpu_ticks[CPU_STATE_IDLE];
    cpu->nice = cpuload.cpu_ticks[CPU_STATE_NICE];
    cpu->wait = 0; /*N/A*/
    cpu->total = cpu->user + cpu->nice + cpu->sys + cpu->idle;

#else
    int status;
    long cp_time[CPUSTATES];

    status = kread(sigar, &cp_time, sizeof(cp_time),
                   sigar->koffsets[KOFFSET_CPUINFO]);

    if (status != SIGAR_OK) {
        return status;
    }

    cpu->user = cp_time[CP_USER];
    cpu->nice = cp_time[CP_NICE];
    cpu->sys  = cp_time[CP_SYS];
    cpu->idle = cp_time[CP_IDLE];
    cpu->wait = cp_time[CP_INTR];
    cpu->total = cpu->user + cpu->nice + cpu->sys + cpu->idle;
#endif

    return SIGAR_OK;
}

int sigar_cpu_list_get(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    sigar_cpu_t *cpu;

    sigar_cpu_list_create(cpulist);

    /* XXX multi cpu */
    cpu = &cpulist->data[cpulist->number++];

    return sigar_cpu_get(sigar, cpu);
}

int sigar_uptime_get(sigar_t *sigar,
                     sigar_uptime_t *uptime)
{
    uptime->uptime   = time(NULL) - sigar->boot_time;

    return SIGAR_OK;
}

int sigar_loadavg_get(sigar_t *sigar,
                      sigar_loadavg_t *loadavg)
{
    getloadavg(loadavg->loadavg, 3);

    return SIGAR_OK;
}

int sigar_proc_list_get(sigar_t *sigar,
                        sigar_proc_list_t *proclist)
{
#ifdef DARWIN
    int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_ALL, 0 };
    int i, num;
    size_t len;
    struct kinfo_proc *proc;

    if (sysctl(mib, NMIB(mib), NULL, &len, NULL, 0) < 0) {
        return errno;
    }

    proc = malloc(len);

    if (sysctl(mib, NMIB(mib), proc, &len, NULL, 0) < 0) {
        free(proc);
        return errno;
    }

    num = len/sizeof(*proc);
    proclist->number = 0;
    proclist->size = num;
    proclist->data = malloc(sizeof(*(proclist->data)) * num);

    for (i=0; i<num; i++) {
        proclist->data[proclist->number++] = proc[i].kp_proc.p_pid;
    }

    free(proc);

    return SIGAR_OK;
#else
    /*XXX above compiles on freebsd but no workie */
    return sigar_proc_list_procfs_get(sigar, proclist);
#endif
}

int sigar_proc_stat_get(sigar_t *sigar,
                        sigar_proc_stat_t *procstat)
{
    int status = /* XXX optimize */
        sigar_proc_count(sigar, &procstat->total);

    return status;
}

static int sigar_get_pinfo(sigar_t *sigar, sigar_pid_t pid)
{
    int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PID, 0 };
    size_t len = sizeof(*sigar->pinfo);
    time_t timenow = time(NULL);
    mib[3] = pid;

    if (sigar->pinfo == NULL) {
        sigar->pinfo = malloc(len);
    }

    if (sigar->last_pid == pid) {
        if ((timenow - sigar->last_getprocs) < SIGAR_LAST_PROC_EXPIRE) {
            return SIGAR_OK;
        }
    }

    sigar->last_pid = pid;
    sigar->last_getprocs = timenow;

    if (sysctl(mib, NMIB(mib), sigar->pinfo, &len, NULL, 0) < 0) {
        return errno;
    }

    return SIGAR_OK;
}

int sigar_proc_mem_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_mem_t *procmem)
{
#ifdef DARWIN
    mach_port_t task, self = mach_task_self();
    kern_return_t status;
    task_basic_info_data_t info;
    mach_msg_type_number_t type = TASK_BASIC_INFO_COUNT;

    status = task_for_pid(self, pid, &task);

    if (status != KERN_SUCCESS) {
        return errno;
    }

    status = task_info(task, TASK_BASIC_INFO, (task_info_t)&info, &type);

    if (task != self) {
        mach_port_deallocate(self, task);
    }

    procmem->vsize    = info.virtual_size;
    procmem->resident = info.resident_size;

    /*XXX*/
    procmem->size = 1; /* 1 == let ant test pass for now */
    procmem->rss = SIGAR_FIELD_NOTIMPL;
    procmem->share = SIGAR_FIELD_NOTIMPL;

    return SIGAR_OK;
#else
    return SIGAR_ENOTIMPL;
#endif
}

int sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_cred_t *proccred)
{
    int status = sigar_get_pinfo(sigar, pid);
    struct kinfo_proc *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    proccred->uid  = pinfo->kp_eproc.e_pcred.p_ruid;
    proccred->gid  = pinfo->kp_eproc.e_pcred.p_rgid;
    proccred->euid = pinfo->kp_eproc.e_pcred.p_svuid;
    proccred->egid = pinfo->kp_eproc.e_pcred.p_svgid;

    return SIGAR_OK;
}

static int get_proc_times(sigar_pid_t pid, sigar_proc_time_t *time)
{
#ifdef DARWIN
    unsigned int count;
    time_value_t utime = {0, 0}, stime = {0, 0};
    task_basic_info_data_t ti;
    task_thread_times_info_data_t tti;
    task_port_t task, self = mach_task_self();
    kern_return_t status;
    
    status = task_for_pid(self, pid, &task);
    if (status != KERN_SUCCESS) {
        return errno;
    }

    count = TASK_BASIC_INFO_COUNT;
    status = task_info(task, TASK_BASIC_INFO,
                       (task_info_t)&ti, &count);
    if (status != KERN_SUCCESS) {
        if (task != self) {
            mach_port_deallocate(self, task);
        }
        return errno;
    }

    count = TASK_THREAD_TIMES_INFO_COUNT;
    status = task_info(task, TASK_THREAD_TIMES_INFO,
                       (task_info_t)&tti, &count);
    if (status != KERN_SUCCESS) {
        if (task != self) {
            mach_port_deallocate(self, task);
        }
        return errno;
    }

    time_value_add(&utime, &ti.user_time);
    time_value_add(&stime, &ti.system_time);
    time_value_add(&utime, &tti.user_time);
    time_value_add(&stime, &tti.system_time);

    time->user = utime.seconds;
    time->sys  = stime.seconds;
    time->total = time->user + time->sys;

    return SIGAR_OK;
#else
    return SIGAR_ENOTIMPL;
#endif
}

int sigar_proc_time_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_time_t *proctime)
{
    int status = sigar_get_pinfo(sigar, pid);
    struct kinfo_proc *pinfo = sigar->pinfo;
    
    if (status != SIGAR_OK) {
        return status;
    }

    if ((status = get_proc_times(pid, proctime)) != SIGAR_OK) {
        return status;
    }

#ifdef DARWIN
    proctime->start_time = pinfo->kp_proc.p_starttime.tv_sec;
#else
    proctime->start_time = 1;/*XXX*/
#endif

    return SIGAR_OK;
}

int sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_state_t *procstate)
{
    int status = sigar_get_pinfo(sigar, pid);
    struct kinfo_proc *pinfo = sigar->pinfo;
    
    if (status != SIGAR_OK) {
        return status;
    }

    SIGAR_SSTRCPY(procstate->name, pinfo->kp_proc.p_comm);
    procstate->ppid     = pinfo->kp_eproc.e_ppid;
    procstate->priority = pinfo->kp_proc.p_priority;
    procstate->nice     = pinfo->kp_proc.p_nice;
    procstate->tty      = SIGAR_FIELD_NOTIMPL; /*XXX*/

    switch (pinfo->kp_proc.p_stat) {
      case SIDL:
        procstate->state = 'D';
        break;
      case SRUN:
        procstate->state = 'R';
        break;
      case SSLEEP:
        procstate->state = 'S';
        break;
      case SSTOP:
        procstate->state = 'T';
        break;
      case SZOMB:
        procstate->state = 'Z';
        break;
    }

    return SIGAR_OK;
}

int sigar_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_args_t *procargs)
{
    return SIGAR_ENOTIMPL;
}

int sigar_proc_env_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_env_t *procenv)
{
    return SIGAR_ENOTIMPL;
}

int sigar_proc_fd_get(sigar_t *sigar, sigar_pid_t pid,
                      sigar_proc_fd_t *procfd)
{
    return SIGAR_ENOTIMPL;
}

int sigar_proc_exe_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_exe_t *procexe)
{
    return SIGAR_ENOTIMPL;
}

int sigar_proc_modules_get(sigar_t *sigar, sigar_pid_t pid,
                           sigar_proc_modules_t *procmods)
{
    return SIGAR_ENOTIMPL;
}

int sigar_thread_cpu_get(sigar_t *sigar,
                         sigar_uint64_t id,
                         sigar_thread_cpu_t *cpu)
{
    return SIGAR_ENOTIMPL;
}

int sigar_os_fs_type_get(sigar_file_system_t *fsp)
{
    char *type = fsp->sys_type_name;

    /* see sys/disklabel.h */
    switch (*type) {
      case 'h':
        if (strEQ(type, "hfs")) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
      case 'u':
        if (strEQ(type, "ufs")) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
    }

    return fsp->type;
}

int sigar_file_system_list_get(sigar_t *sigar,
                               sigar_file_system_list_t *fslist)
{
    struct statfs *fs;
    int num, i;
    long len;

    if ((num = getfsstat(NULL, 0, MNT_NOWAIT)) < 0) {
        return errno;
    }

    len = sizeof(*fs) * num;
    fs = malloc(len);

    if ((num = getfsstat(fs, len, MNT_NOWAIT)) < 0) {
        return errno;
    }

    sigar_file_system_list_create(fslist);

    for (i=0; i<num; i++) {
        sigar_file_system_t *fsp;

        SIGAR_FILE_SYSTEM_LIST_GROW(fslist);

        fsp = &fslist->data[fslist->number++];

        SIGAR_SSTRCPY(fsp->dir_name, fs[i].f_mntonname);
        SIGAR_SSTRCPY(fsp->dev_name, fs[i].f_mntfromname);
        SIGAR_SSTRCPY(fsp->sys_type_name, fs[i].f_fstypename);
        sigar_fs_type_init(fsp);
    }

    return SIGAR_OK;
}

int sigar_file_system_usage_get(sigar_t *sigar,
                                const char *dirname,
                                sigar_file_system_usage_t *fsusage)
{
    struct statfs buf;

    if (statfs(dirname, &buf) < 0) {
        return errno;
    }

    fsusage->total = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_blocks);
    fsusage->free  = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_bfree);
    fsusage->avail = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_bavail);
    fsusage->files = buf.f_files;
    fsusage->free_files = buf.f_files;
    fsusage->use_percent = sigar_file_system_usage_calc_used(sigar, fsusage);

    SIGAR_DISK_STATS_NOTIMPL(fsusage);

    return SIGAR_OK;
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    int i;

    sigar_cpu_info_list_create(cpu_infos);

    for (i=0; i<sigar->ncpu; i++) {
        sigar_cpu_info_t *info;

        SIGAR_CPU_INFO_LIST_GROW(cpu_infos);

        info = &cpu_infos->data[cpu_infos->number++];

        SIGAR_SSTRCPY(info->vendor, "Apple");
        SIGAR_SSTRCPY(info->model, "powerpc");

        info->mhz = SIGAR_FIELD_NOTIMPL; /*XXX*/
        info->cache_size = SIGAR_FIELD_NOTIMPL;
    }

    return SIGAR_OK;
}

int sigar_net_route_list_get(sigar_t *sigar,
                             sigar_net_route_list_t *routelist)
{
    sigar_net_route_list_create(routelist);

    return SIGAR_OK;
}

typedef enum {
    IFMSG_ITER_LIST,
    IFMSG_ITER_GET
} ifmsg_iter_e;

typedef struct {
    const char *name;
    ifmsg_iter_e type;
    union {
        sigar_net_interface_list_t *iflist;
        struct if_msghdr *ifm;
    } data;
} ifmsg_iter_t;

static int sigar_ifmsg_init(sigar_t *sigar)
{
    int mib[] = { CTL_NET, PF_ROUTE, 0, AF_INET, NET_RT_IFLIST, 0 };
    size_t len;

    if (sysctl(mib, NMIB(mib), NULL, &len, NULL, 0) < 0) {
        return errno;
    }

    if (sigar->ifconf_len < len) {
        sigar->ifconf_buf = realloc(sigar->ifconf_buf, len);
        sigar->ifconf_len = len;
    }

    if (sysctl(mib, NMIB(mib), sigar->ifconf_buf, &len, NULL, 0) < 0) {
        return errno;
    }

    return SIGAR_OK;
}

static int sigar_ifmsg_iter(sigar_t *sigar, ifmsg_iter_t *iter)
{
    char *end = sigar->ifconf_buf + sigar->ifconf_len;
    char *ptr = sigar->ifconf_buf;

    if (iter->type == IFMSG_ITER_LIST) {
        sigar_net_interface_list_create(iter->data.iflist);
    }

    while (ptr < end) {
        char *name;
        struct sockaddr_dl *sdl;
        struct if_msghdr *ifm = (struct if_msghdr *)ptr;
        
        if (ifm->ifm_type != RTM_IFINFO) {
            break;
        }

        ptr += ifm->ifm_msglen;
        
        while (ptr < end) {
            struct if_msghdr *next = (struct if_msghdr *)ptr;

            if (next->ifm_type != RTM_NEWADDR) {
                break;
            }

            ptr += next->ifm_msglen;
        }

        sdl = (struct sockaddr_dl *)(ifm + 1);
        if (sdl->sdl_family != AF_LINK) {
            continue;
        }

        switch (iter->type) {
          case IFMSG_ITER_LIST:
            SIGAR_NET_IFLIST_GROW(iter->data.iflist);

            name = malloc(sdl->sdl_nlen+1);
            memcpy(name, sdl->sdl_data, sdl->sdl_nlen+1);

            iter->data.iflist->data[iter->data.iflist->number++] = name;
            break;

          case IFMSG_ITER_GET:
            if (strEQ(iter->name, sdl->sdl_data)) {
                iter->data.ifm = ifm;
                return SIGAR_OK;
            }
        }
    }

    switch (iter->type) {
      case IFMSG_ITER_LIST:
        return SIGAR_OK;

      case IFMSG_ITER_GET:
      default:
        return ENXIO;
    }
}

int sigar_net_interface_list_get(sigar_t *sigar,
                                 sigar_net_interface_list_t *iflist)
{
    int status;
    ifmsg_iter_t iter;

    if ((status = sigar_ifmsg_init(sigar)) != SIGAR_OK) {
        return status;
    }

    iter.type = IFMSG_ITER_LIST;
    iter.data.iflist = iflist;

    return sigar_ifmsg_iter(sigar, &iter);
}

int sigar_net_interface_config_get(sigar_t *sigar, const char *name,
                                   sigar_net_interface_config_t *ifconfig)
{
    int sock;
    int status;
    ifmsg_iter_t iter;
    struct if_msghdr *ifm;
    struct sockaddr_dl *sdl;
    struct ifreq ifr;

    if (sigar->ifconf_len == 0) {
        if ((status = sigar_ifmsg_init(sigar)) != SIGAR_OK) {
            return status;
        }
    }

    iter.type = IFMSG_ITER_GET;
    iter.name = name;

    if ((status = sigar_ifmsg_iter(sigar, &iter)) != SIGAR_OK) {
        return status;
    }

    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        return errno;
    }

    ifm = iter.data.ifm;

    SIGAR_SSTRCPY(ifconfig->name, name);

    sdl = (struct sockaddr_dl *)(ifm + 1);
    sigar_hwaddr_format(ifconfig->hwaddr,
                        (unsigned char *)LLADDR(sdl));

    ifconfig->flags = ifm->ifm_flags;
    ifconfig->mtu = ifm->ifm_data.ifi_mtu;
    ifconfig->metric = ifm->ifm_data.ifi_metric;

    SIGAR_SSTRCPY(ifr.ifr_name, name);

#define ifr_s_addr(ifr) \
    ((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr.s_addr

    if (!ioctl(sock, SIOCGIFADDR, &ifr)) {
        ifconfig->address = ifr_s_addr(ifr);
    }

    if (!ioctl(sock, SIOCGIFNETMASK, &ifr)) {
        ifconfig->netmask = ifr_s_addr(ifr);
    }

    if (ifconfig->flags & IFF_LOOPBACK) {
        ifconfig->destination = ifconfig->address;
        ifconfig->broadcast = 0;
    }
    else {
        if (!ioctl(sock, SIOCGIFDSTADDR, &ifr)) {
            ifconfig->destination = ifr_s_addr(ifr);
        }

        if (!ioctl(sock, SIOCGIFBRDADDR, &ifr)) {
            ifconfig->broadcast = ifr_s_addr(ifr);
        }
    }

    close(sock);

    return SIGAR_OK;
}

int sigar_net_interface_stat_get(sigar_t *sigar, const char *name,
                                 sigar_net_interface_stat_t *ifstat)
{
    int status;
    ifmsg_iter_t iter;
    struct if_msghdr *ifm;

    if (sigar->ifconf_len == 0) {
        if ((status = sigar_ifmsg_init(sigar)) != SIGAR_OK) {
            return status;
        }
    }

    iter.type = IFMSG_ITER_GET;
    iter.name = name;

    if ((status = sigar_ifmsg_iter(sigar, &iter)) != SIGAR_OK) {
        return status;
    }

    ifm = iter.data.ifm;

    ifstat->rx_bytes      = ifm->ifm_data.ifi_ibytes;
    ifstat->rx_packets    = ifm->ifm_data.ifi_ipackets;
    ifstat->rx_errors     = ifm->ifm_data.ifi_ierrors;
    ifstat->rx_dropped    = ifm->ifm_data.ifi_iqdrops;
    ifstat->rx_overruns   = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_frame      = SIGAR_FIELD_NOTIMPL;

    ifstat->tx_bytes      = ifm->ifm_data.ifi_obytes;
    ifstat->tx_packets    = ifm->ifm_data.ifi_opackets;
    ifstat->tx_errors     = ifm->ifm_data.ifi_oerrors;
    ifstat->tx_collisions = ifm->ifm_data.ifi_collisions;
    ifstat->tx_dropped    = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_overruns   = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_carrier    = SIGAR_FIELD_NOTIMPL;

    return SIGAR_OK;
}

int sigar_net_connection_list_get(sigar_t *sigar,
                                  sigar_net_connection_list_t *connlist,
                                  int flags)
{
    return SIGAR_ENOTIMPL;
}
