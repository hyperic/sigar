#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"
#include "sigar_util.h"

int sigar_os_open(sigar_t **sigar)
{
    *sigar = malloc(sizeof(**sigar));

    (*sigar)->pagesize = getpagesize();
    (*sigar)->boot_time = 0;

    return SIGAR_OK;
}

int sigar_os_close(sigar_t *sigar)
{
    free(sigar);
    return SIGAR_OK;
}

char *sigar_os_error_string(int err)
{
    return NULL;
}

int sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
    vm_statistics_data_t vmstats;
    
    vm_statistics(task_self(), &vmstats);

    mem->free = vmstats.free_count   * vmstats.pagesize;
    mem->used = vmstats.active_count * vmstats.pagesize;
    
    mem->total =
        mem->free + mem->used +
        ((vmstats.inactive_count + vmstats.wire_count) * vmstats.pagesize);

    sigar_mem_calc_ram(sigar, mem);

    mem->shared = -1;
    mem->buffer = -1;
    mem->cached = -1;

    return SIGAR_OK;
}

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
    struct tbl_swapinfo info;
   
    table(TBL_SWAPINFO, -1, &info, 1, sizeof(info));

    swap->total  = info.size;
    swap->free   = info.free;
    swap->total *= sigar->pagesize;
    swap->free  *= sigar->pagesize;

    swap->used   = swap->total - swap->free;

    return SIGAR_OK;
}

int sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
    struct tbl_sysinfo sysinfo;
    
    if (table(TBL_SYSINFO, 0, &sysinfo, 1, sizeof(sysinfo)) != 1) {
        return errno;
    }

    cpu->user = sysinfo.si_user;
    cpu->nice = sysinfo.si_nice;
    cpu->sys  = sysinfo.si_sys;
    cpu->idle = sysinfo.si_idle;

    cpu->total = cpu->user + cpu->nice + cpu->sys + cpu->idle;

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
    if (sigar->boot_time == 0) {
        struct tbl_sysinfo sysinfo;
    
        if (table(TBL_SYSINFO, 0, &sysinfo, 1, sizeof(sysinfo)) != 1) {
            return errno;
        }

        sigar->boot_time = sysinfo.si_boottime;
    }

    uptime->uptime = time(NULL) - sigar->boot_time;

    return SIGAR_OK;
}

int sigar_loadavg_get(sigar_t *sigar,
                      sigar_loadavg_t *loadavg)
{
    int i;
    struct tbl_loadavg avg;

    if (table(TBL_LOADAVG, 0, &avg, 1, sizeof(avg)) < 0) {
        return errno;
    }

    if (avg.tl_lscale) {
        for (i=0; i<3; i++) {
            loadavg->loadavg[i] =
                ((double)avg.tl_avenrun.l[i] / 
                 (double)avg.tl_lscale);
        }
    }
    else {
        for (i=0; i<3; i++) {
            loadavg->loadavg[i] = avg.tl_avenrun.d[i];
        }
    }

    return SIGAR_OK;
}

int sigar_proc_list_get(sigar_t *sigar,
                        sigar_proc_list_t *proclist)
{
    return SIGAR_ENOTIMPL;
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
    procmem->size = -1;
    procmem->vsize = -1;
    procmem->share = -1;
    procmem->rss = -1;
    procmem->resident = -1;

    return SIGAR_OK;
}

int sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_cred_t *proccred)
{
    proccred->uid = -1;
    proccred->gid = -1;
    proccred->euid = -1;
    proccred->egid = -1;

    return SIGAR_OK;
}

int sigar_proc_time_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_time_t *proctime)
{
    proctime->start_time = -1;
    proctime->user = -1;
    proctime->sys  = -1;
    proctime->total = proctime->user + proctime->sys;

    return SIGAR_OK;
}

int sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_state_t *procstate)
{
    SIGAR_SSTRCPY(procstate->name, "java");
    procstate->ppid = -1;
    procstate->priority = -1;
    procstate->nice = -1;
    procstate->tty = -1;
    procstate->state = 'R';

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
    procfd->total = -1;
    return SIGAR_OK;
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

int sigar_os_fs_type_get(sigar_file_system_t *fsp)
{
    fsp->type = SIGAR_FSTYPE_UNKNOWN;

    return SIGAR_OK;
}

int sigar_file_system_list_get(sigar_t *sigar,
                               sigar_file_system_list_t *fslist)
{
    sigar_file_system_list_create(fslist);

    return SIGAR_OK;
}

int sigar_file_system_usage_get(sigar_t *sigar,
                                const char *dirname,
                                sigar_file_system_usage_t *fsusage)
{
    fsusage->total = -1;
    fsusage->free  = -1;
    fsusage->avail = -1;
    fsusage->files = -1;
    fsusage->free_files = -1;

    return SIGAR_OK;
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    sigar_cpu_info_t *info;

    sigar_cpu_info_list_create(cpu_infos);

    info = &cpu_infos->data[cpu_infos->number++];

    SIGAR_SSTRCPY(info->vendor, "vendor");
    SIGAR_SSTRCPY(info->model, "model");
    info->mhz = -1;
    info->cache_size = -1;

    return SIGAR_OK;
}

int sigar_net_route_list_get(sigar_t *sigar,
                             sigar_net_route_list_t *routelist)
{
    return SIGAR_ENOTIMPL;
}

int sigar_net_interface_stat_get(sigar_t *sigar, const char *name,
                                 sigar_net_interface_stat_t *ifstat)
{
    ifstat->rx_bytes      = -1;
    ifstat->rx_packets    = -1;
    ifstat->rx_errors     = -1;
    ifstat->rx_dropped    = -1;
    ifstat->rx_overruns   = -1;
    ifstat->rx_frame      = -1;

    ifstat->tx_bytes      = -1;
    ifstat->tx_packets    = -1;
    ifstat->tx_errors     = -1;
    ifstat->tx_dropped    = -1;
    ifstat->tx_overruns   = -1;
    ifstat->tx_collisions = -1;
    ifstat->tx_carrier    = -1;

    return SIGAR_OK;
}

int sigar_net_connection_list_get(sigar_t *sigar,
                                  sigar_net_connection_list_t *connlist,
                                  int flags)
{
    return SIGAR_ENOTIMPL;
}
