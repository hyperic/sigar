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
#include "sigar_os.h"
#include "sigar_util.h"

#include <mach.h>
#include <mach/mach_types.h>
#include <mach/task_info.h>

#include <sys/mount.h>
#include <sys/fs_types.h>
#include <sys/user.h>

int sigar_os_open(sigar_t **sigar)
{
    *sigar = malloc(sizeof(**sigar));

    (*sigar)->pagesize = getpagesize();
    (*sigar)->boot_time = 0;
    (*sigar)->mhz = 0;
    (*sigar)->nproc = -1;

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
    vm_statistics_data_t vmstats;
    
    vm_statistics(task_self(), &vmstats);

    mem->free = vmstats.free_count   * vmstats.pagesize;
    mem->used = vmstats.active_count * vmstats.pagesize;
    
    mem->total =
        mem->free + mem->used +
        ((vmstats.inactive_count + vmstats.wire_count) * vmstats.pagesize);

    mem->actual_free = mem->free;
    mem->actual_used = mem->used;

    sigar_mem_calc_ram(sigar, mem);

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

    swap->page_in = swap->page_out = -1;

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
    cpu->wait = 0; /*N/A?*/
    cpu->irq = 0; /*N/A*/
    cpu->soft_irq = 0; /*N/A*/
    cpu->stolen = 0; /*N/A*/
    cpu->total = cpu->user + cpu->nice + cpu->sys + cpu->idle + cpu->wait;

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

#define PROC_ELTS 16

int sigar_os_proc_list_get(sigar_t *sigar,
                           sigar_proc_list_t *proclist)
{
    struct tbl_procinfo procinfo[PROC_ELTS];
    int offset;

    if (sigar->nproc == -1) {
        /* this number will not change while we are running */
        sigar->nproc = table(TBL_PROCINFO, 0, NULL, INT_MAX, 0);
    }

    for (offset=0; offset<sigar->nproc; offset+=PROC_ELTS) {
        int i;
        int elts = table(TBL_PROCINFO, offset, &procinfo,
                         PROC_ELTS, sizeof(procinfo[0]));

        for (i=0; i<elts; i++) {
            struct tbl_procinfo *info = &procinfo[i];
            if (info->pi_status == PI_EMPTY) {
                continue;
            }

            SIGAR_PROC_LIST_GROW(proclist);
            
            proclist->data[proclist->number++] = info->pi_pid;
        }
    }

    return SIGAR_OK;
}

int sigar_proc_mem_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_mem_t *procmem)
{
    task_t self;
    task_basic_info_data_t taskinfo;
    struct user s_user;
    int type = TASK_BASIC_INFO_COUNT;
    int status;

    status = task_by_unix_pid(task_self(), pid, &self);

    if (status != KERN_SUCCESS) {
        return errno;
    }

    status = task_info(self, TASK_BASIC_INFO,
                       (task_info_t)&taskinfo, &type);

    if (status != KERN_SUCCESS) {
        return errno;
    }

    procmem->resident = taskinfo.resident_size;
    procmem->size    = taskinfo.virtual_size;

    status = table(TBL_UAREA, pid, &s_user, 1, sizeof(s_user));

    if (status != 1) {
        procmem->share = SIGAR_FIELD_NOTIMPL;
        return SIGAR_OK;
    }

    procmem->share = s_user.u_ru.ru_ixrss;

    procmem->page_faults  = SIGAR_FIELD_NOTIMPL;
    procmem->minor_faults = SIGAR_FIELD_NOTIMPL;
    procmem->major_faults = SIGAR_FIELD_NOTIMPL;

    return SIGAR_OK;
}

int sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_cred_t *proccred)
{
    proccred->uid = SIGAR_FIELD_NOTIMPL;
    proccred->gid = SIGAR_FIELD_NOTIMPL;
    proccred->euid = SIGAR_FIELD_NOTIMPL;
    proccred->egid = SIGAR_FIELD_NOTIMPL;

    return SIGAR_OK;
}

int sigar_proc_time_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_time_t *proctime)
{
    struct user s_user;
    int status;

    status = table(TBL_UAREA, pid, &s_user, 1, sizeof(s_user));

    if (status != 1) {
        return errno;
    }

    proctime->user  = s_user.u_ru.ru_utime.tv_sec;
    proctime->sys   = s_user.u_ru.ru_stime.tv_sec;
    proctime->total = proctime->user + proctime->sys;
    proctime->start_time = s_user.u_start.tv_sec;

    return SIGAR_OK;
}

int sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_state_t *procstate)
{
    struct tbl_procinfo info;
    int status;

    status = table(TBL_PROCINFO, pid, &info, 1, sizeof(info));

    if (status != 1) {
        return errno;
    }

    SIGAR_SSTRCPY(procstate->name, info.pi_comm);
    procstate->ppid = info.pi_ppid;
    procstate->priority = SIGAR_FIELD_NOTIMPL;
    procstate->nice = SIGAR_FIELD_NOTIMPL;
    procstate->tty = info.pi_ttyd;
    procstate->threads = SIGAR_FIELD_NOTIMPL;
    procstate->processor = SIGAR_FIELD_NOTIMPL;

    switch (info.pi_status) {
      case PI_ACTIVE:
        procstate->state = 'R';
        break;
      case PI_ZOMBIE:
        procstate->state = 'Z';
        break;
      default:
        procstate->state = 'S';
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
    procfd->total = SIGAR_FIELD_NOTIMPL;
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
    return fsp->type;
}

static int sigar_fsstat(struct statfs **fs, int *num)
{
    int size;

    if ((*num = getfsstat(NULL, 0, MNT_WAIT)) < 0) {
        return errno;
    }

    size = ((*num)+1) * sizeof(struct statfs);

    *fs = malloc(size);

    if ((*num = getfsstat(*fs, size, MNT_WAIT)) < 0) {
        free(fs);
        return errno;
    }

    return SIGAR_OK;
}

int sigar_file_system_list_get(sigar_t *sigar,
                               sigar_file_system_list_t *fslist)
{
    int i, num, status;
    struct statfs *fs;

    if ((status = sigar_fsstat(&fs, &num)) != SIGAR_OK) {
        return status;
    }

    sigar_file_system_list_create(fslist);

    for (i=0; i<num; i++) {
        sigar_file_system_t *fsp;
        const char *typename = NULL;

        SIGAR_FILE_SYSTEM_LIST_GROW(fslist);

        fsp = &fslist->data[fslist->number++];

        SIGAR_SSTRCPY(fsp->dir_name, fs[i].f_mntonname);
        SIGAR_SSTRCPY(fsp->dev_name, fs[i].f_mntfromname);
        SIGAR_SSTRCPY(fsp->sys_type_name, mnt_names[fs[i].f_type]);
        fsp->options[0] = '\0'; /*XXX*/

        switch (fs[i].f_type) {
          case MOUNT_UFS:
            typename = "ufs";
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
            break;
          case MOUNT_MFS:
          case MOUNT_MSFS:
            typename = "advfs";
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
            break;
          case MOUNT_FDFS:
            typename = "fdfs";
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
            break;
          case MOUNT_DVDFS:
          case MOUNT_CDFS:
            fsp->type = SIGAR_FSTYPE_CDROM;
            break;
          case MOUNT_DFS: /* DCE */
            typename = "dfs";
            fsp->type = SIGAR_FSTYPE_NETWORK;
            break;
          case MOUNT_EFS: /* DCE */
            typename = "efs";
            fsp->type = SIGAR_FSTYPE_NETWORK;
            break;
          case MOUNT_NFS:
          case MOUNT_NFS3:
            typename = "nfs";
            fsp->type = SIGAR_FSTYPE_NETWORK;
            break;
          case MOUNT_CSPEC:
          case MOUNT_CFS:
            typename = "cfs";
            fsp->type = SIGAR_FSTYPE_NETWORK;
            break;
          case MOUNT_NONE:
          case MOUNT_PC:
          case MOUNT_S5FS:
          case MOUNT_PROCFS:
          case MOUNT_FFM:
          case MOUNT_ADDON:
          case MOUNT_AUTOFS:
          default:
            break;
        }

        /* we set fsp->type, just looking up sigar.c:fstype_names[type] */
        sigar_fs_type_get(fsp);

        if (typename == NULL) {
            typename = fsp->type_name;
        }
    }

    free(fs);

    return SIGAR_OK;
}

int sigar_disk_usage_get(sigar_t *sigar, const char *name,
                         sigar_disk_usage_t *usage)
{
    return SIGAR_ENOTIMPL;
}

#define SIGAR_FS_BLOCKS_TO_BYTES(buf, f) \
    (((sigar_uint64_t)buf.f * (buf.f_bsize / 512)) >> 1)

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
    fsusage->used  = fsusage->total - fsusage->free;
    fsusage->files = buf.f_files;
    fsusage->free_files = buf.f_ffree;
    fsusage->use_percent = sigar_file_system_usage_calc_used(sigar, fsusage);

    SIGAR_DISK_STATS_INIT(&fsusage->disk)

    return SIGAR_OK;
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    sigar_cpu_info_t *info;

    if (sigar->mhz == 0) {
        struct tbl_sysinfo sysinfo;
    
        if (table(TBL_SYSINFO, 0, &sysinfo, 1, sizeof(sysinfo)) != 1) {
            return errno;
        }

        sigar->mhz = sysinfo.si_hz;
    }

    sigar_cpu_info_list_create(cpu_infos);

    info = &cpu_infos->data[cpu_infos->number++];

    SIGAR_SSTRCPY(info->vendor, "DEC");
    SIGAR_SSTRCPY(info->model, "alpha");
    info->mhz = sigar->mhz;
    info->cache_size = SIGAR_FIELD_NOTIMPL;

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
    ifstat->rx_bytes      = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_packets    = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_errors     = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_dropped    = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_overruns   = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_frame      = SIGAR_FIELD_NOTIMPL;

    ifstat->tx_bytes      = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_packets    = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_errors     = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_dropped    = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_overruns   = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_collisions = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_carrier    = SIGAR_FIELD_NOTIMPL;

    ifstat->speed         = SIGAR_FIELD_NOTIMPL;

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
