#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"
#include "sigar_util.h"

int sigar_os_open(sigar_t **sigar)
{
    *sigar = malloc(sizeof(**sigar));

    return SIGAR_OK;
}

int sigar_os_close(sigar_t *sigar)
{
    free(sigar);
    return SIGAR_OK;
}

char *sigar_os_error_string(sigar_t *sigar, int err)
{
    return NULL;
}

int sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
    mem->total  = -1;
    mem->ram    = -1;
    mem->used   = -1;
    mem->free   = -1;
    mem->actual_free = mem->free;
    mem->actual_used = mem->used;
    return SIGAR_OK;
}

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
    swap->total  = -1;
    swap->used   = -1;
    swap->free   = -1;
    swap->page_in = -1;
    swap->page_out = -1;

    return SIGAR_OK;
}

int sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
    cpu->user = -1;
    cpu->nice = -1;
    cpu->sys  = -1;
    cpu->idle = -1;
    cpu->wait = -1;

    cpu->total = cpu->user + cpu->nice + cpu->sys + cpu->idle;

    return SIGAR_OK;
}

int sigar_cpu_list_get(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    return SIGAR_ENOTIMPL;
}

int sigar_uptime_get(sigar_t *sigar,
                     sigar_uptime_t *uptime)
{
    uptime->uptime   = -1;

    return SIGAR_OK;
}

int sigar_loadavg_get(sigar_t *sigar,
                      sigar_loadavg_t *loadavg)
{
    loadavg->loadavg[0] = -1;
    loadavg->loadavg[1] = -1;
    loadavg->loadavg[2] = -1;

    return SIGAR_OK;
}

int sigar_proc_list_get(sigar_t *sigar,
                        sigar_proc_list_t *proclist)
{
    return SIGAR_ENOTIMPL;
}

int sigar_proc_mem_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_mem_t *procmem)
{
    procmem->size = -1;
    procmem->share = -1;
    procmem->resident = -1;
    procmem->page_faults  = -1;
    procmem->minor_faults = -1;
    procmem->major_faults = -1;

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
    procstate->threads = -1;
    procstate->processor = -1;

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

int sigar_thread_cpu_get(sigar_t *sigar,
                         sigar_uint64_t id,
                         sigar_thread_cpu_t *cpu)
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

int sigar_disk_usage_get(sigar_t *sigar, const char *name,
                         sigar_disk_usage_t *usage)
{
    return SIGAR_ENOTIMPL;
}

int sigar_file_system_usage_get(sigar_t *sigar,
                                const char *dirname,
                                sigar_file_system_usage_t *fsusage)
{
    fsusage->total = -1;
    fsusage->free  = -1;
    fsusage->avail = -1;
    fsusage->used  = -1;
    fsusage->files = -1;
    fsusage->free_files = -1;
    SIGAR_DISK_STATS_INIT(&fsusage->disk);

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
    ifstat->speed         = -1;

    return SIGAR_OK;
}

int sigar_net_connection_walk(sigar_net_connection_walker_t *walker)
{
    return SIGAR_ENOTIMPL;
}

SIGAR_DECLARE(int)
sigar_tcp_get(sigar_t *sigar,
              sigar_tcp_t *tcp)
{
    return SIGAR_ENOTIMPL;
}

int sigar_nfs_client_v2_get(sigar_t *sigar,
                            sigar_nfs_client_v2_t *nfs)
{
    return SIGAR_ENOTIMPL;
}

int sigar_nfs_server_v2_get(sigar_t *sigar,
                            sigar_nfs_server_v2_t *nfs)
{
    return SIGAR_ENOTIMPL;
}

int sigar_nfs_client_v3_get(sigar_t *sigar,
                            sigar_nfs_client_v3_t *nfs)
{
    return SIGAR_ENOTIMPL;
}

int sigar_nfs_server_v3_get(sigar_t *sigar,
                            sigar_nfs_server_v3_t *nfs)
{
    return SIGAR_ENOTIMPL;
}

int sigar_proc_port_get(sigar_t *sigar, int protocol,
                        unsigned long port, sigar_pid_t *pid)
{
    return SIGAR_ENOTIMPL;
}

int sigar_os_sys_info_get(sigar_t *sigar,
                          sigar_sys_info_t *sysinfo)
{
    return SIGAR_OK;
}
