#define WIN32_LEAN_AND_MEAN

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"
#include "sigar_util.h"

#include <errno.h>
#include <monitor.h>
#include <stdio.h>
#include <windows.h>
#include <netware.h>
#include <novsock2.h>
#include <ws2tcpip.h>
#include <sys/statfs.h>
#define _STRUCT_TM
#include <nwtime.h>
#include <nit/nwservst.h>

/*
 * http://developer.novell.com/research/appnotes/2003/may/05/a0305058.htm
 */
int _NonAppStart(void *NLMHandle,
                 void        *errorScreen,
                 const char  *cmdLine,
                 const char  *loadDirPath,
                 size_t      uninitializedDataLength,
                 void        *NLMFileHandle,
                 int         (*readRoutineP)(int conn,
                                             void *fileHandle,
                                             size_t offset,
                                             size_t nbytes,
                                             size_t *bytesRead,
                                             void *buffer),
                 size_t      customDataOffset,
                 size_t      customDataSize,
                 int         messageCount,
                 const char  **messages)
{

#pragma unused(cmdLine)
#pragma unused(loadDirPath)
#pragma unused(uninitializedDataLength)
#pragma unused(NLMFileHandle)
#pragma unused(readRoutineP)
#pragma unused(customDataOffset)
#pragma unused(customDataSize)
#pragma unused(messageCount)
#pragma unused(messages)

    WSADATA wsaData;

    NWCallsInit(NULL, NULL);

    return WSAStartup((WORD)MAKEWORD(2, 0), &wsaData);
}

void _NonAppStop(void)
{
    NWCallsTerm(NULL);
    WSACleanup();
}

int _NonAppCheckUnload(void)
{
    return 0;
}

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
    struct memory_info info;
    if (netware_mem_info(&info) != 0) {
        return errno;
    }
    mem->total  = info.TotalKnownSystemMemoryUnder4Gb;
    mem->used   = info.TotalWorkMemory;
    mem->free   = mem->total - mem->used;
    mem->shared = -1;
    mem->actual_free = mem->free;
    mem->actual_used = mem->used;

    sigar_mem_calc_ram(sigar, mem);

    return SIGAR_OK;
}

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
    struct vmemory_info info;

    if (netware_vmem_info(&info) != 0) {
        return errno;
    }

    swap->used  = info.SwapPageCount * PAGESIZE;
    swap->free  = info.SwapFreeCount * PAGESIZE;
    swap->total = swap->used + swap->free;

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
    LONG seconds, tenths;

    TicksToSeconds(GetCurrentTicks(), &seconds, &tenths);
    uptime->uptime = seconds;

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
    struct volume_info info;
    int num = 0;

    sigar_file_system_list_create(fslist);

    while (netware_vol_info(&info, &num) == 0) {
        sigar_file_system_t *fsp;
        int len;
        char *type = NULL;

        SIGAR_FILE_SYSTEM_LIST_GROW(fslist);

        fsp = &fslist->data[fslist->number++];
        SIGAR_SSTRCPY(fsp->dev_name, info.name);
        SIGAR_SSTRCPY(fsp->dir_name, info.name);
        len = strlen(info.name);
        fsp->dir_name[len] = ':';
        fsp->dir_name[len+1] = '/';
        fsp->dir_name[len+2] = '\0';

        fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        type = "nss";

        sigar_fs_type_get(fsp);
        if (!type) {
            type = fsp->type_name;
        }

        SIGAR_SSTRCPY(fsp->sys_type_name, type);
    }

    return SIGAR_OK;
}

#define SIGAR_FS_BLOCKS_TO_BYTES(buf, f) \
    ((buf.f * (buf.f_bsize / 512)) >> 1)

int sigar_file_system_usage_get(sigar_t *sigar,
                                const char *dirname,
                                sigar_file_system_usage_t *fsusage)
{
    struct statfs buf;

    if (statfs(dirname, &buf) != 0) {
        return errno;
    }

    fsusage->total = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_blocks);
    fsusage->free  = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_bfree);
    fsusage->avail = fsusage->free;
    fsusage->used  = fsusage->total - fsusage->free;
    fsusage->files = buf.f_files;
    fsusage->free_files = buf.f_ffree;
    fsusage->use_percent = sigar_file_system_usage_calc_used(sigar, fsusage);

    SIGAR_DISK_STATS_NOTIMPL(fsusage);

    return SIGAR_OK;
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    struct cpu_info cpu;
    int num = 0;

    sigar_cpu_info_list_create(cpu_infos);

    while (netware_cpu_info(&cpu, &num) == 0) {
        sigar_cpu_info_t *info;

        SIGAR_CPU_INFO_LIST_GROW(cpu_infos);

        info = &cpu_infos->data[cpu_infos->number++];

        SIGAR_SSTRCPY(info->vendor, "vendor");
        SIGAR_SSTRCPY(info->model, "model");
        info->mhz = cpu.Speed;
        info->cache_size = cpu.L2CacheSize;
    }

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
    LONG board = 1; /* XXX derive from name */
    LONG block = 0;
    BYTE buffer[SS_DEFAULT_BUFFER_SIZE];
    WORD len = sizeof(buffer);
    CommonLANStructure *info;

    if (SSGetLANCommonCounters(board, block, buffer, len) != 0) {
        return ENOENT;
    }

    info = &((GetLANCommonCountersStructure *)buffer)->info;

    ifstat->rx_bytes      = info->TotalTxOKByteCountHigh;
    ifstat->rx_packets    = info->TotalRxPacketCount;
    ifstat->rx_errors     = info->PacketRxMiscErrorCount;
    ifstat->rx_dropped    = -1;
    ifstat->rx_overruns   = info->PacketRxTooBigCount;
    ifstat->rx_frame      = -1;

    ifstat->tx_bytes      = info->TotalTxOKByteCountHigh;
    ifstat->tx_packets    = info->TotalTxPacketCount;
    ifstat->tx_errors     = info->PacketTxMiscErrorCount;
    ifstat->tx_dropped    = info->RetryTxCount;
    ifstat->tx_overruns   = info->PacketTxTooBigCount;
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

int sigar_net_info_get(sigar_t *sigar,
                       sigar_net_info_t *netinfo)
{
    return SIGAR_ENOTIMPL;
}

int sigar_proc_port_get(sigar_t *sigar, int protocol,
                        unsigned long port, sigar_pid_t *pid)
{
    return SIGAR_ENOTIMPL;
}

int sigar_nfs_ping(char *host)
{
    return SIGAR_ENOTIMPL;
}
