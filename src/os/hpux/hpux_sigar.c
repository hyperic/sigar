#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

#include <sys/dk.h>
#include <sys/lwp.h>
#include <sys/stat.h>
#include <errno.h>

int sigar_os_open(sigar_t **sigar)
{
    *sigar = malloc(sizeof(**sigar));

    /* does not change while system is running */
    pstat_getstatic(&(*sigar)->pstatic,
                    sizeof((*sigar)->pstatic),
                    1, 0);

    (*sigar)->ticks = sysconf(_SC_CLK_TCK);

    (*sigar)->last_pid = -1;

    (*sigar)->pinfo = NULL;

    (*sigar)->fsdev = NULL;

    (*sigar)->mib = -1;
    
    return SIGAR_OK;
    
}

int sigar_os_close(sigar_t *sigar)
{
    if (sigar->pinfo) {
        free(sigar->pinfo);
    }
    if (sigar->fsdev) {
        sigar_cache_destroy(sigar->fsdev);
    }
    if (sigar->mib >= 0) {
        close_mib(sigar->mib);
    } 
    free(sigar);
    return SIGAR_OK;
}

char *sigar_os_error_string(sigar_t *sigar, int err)
{
    return NULL;
}

int sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
    struct pst_dynamic stats;
    sigar_uint64_t pagesize = sigar->pstatic.page_size;

    mem->total = sigar->pstatic.physical_memory * pagesize;

    sigar_mem_calc_ram(sigar, mem);

    pstat_getdynamic(&stats, sizeof(stats), 1, 0);

    mem->free = stats.psd_free * pagesize;
    mem->used = mem->total - mem->free;

    /*XXX*/
    mem->shared = SIGAR_FIELD_NOTIMPL;

    mem->actual_free = mem->free;
    mem->actual_used = mem->used;
    
    return SIGAR_OK;
}

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
    struct pst_swapinfo swapinfo;
    int i=0;

    swap->total = swap->free = 0;

    while (pstat_getswap(&swapinfo, sizeof(swapinfo), 1, i++) > 0) {
        swapinfo.pss_nfpgs *= 4;  /* nfpgs is in 512 byte blocks */

        if (swapinfo.pss_nblksenabled == 0) {
            swapinfo.pss_nblksenabled = swapinfo.pss_nfpgs;
        }

        swap->total += swapinfo.pss_nblksenabled;
        swap->free  += swapinfo.pss_nfpgs;
    }

    swap->used = swap->total - swap->free;
    
    return SIGAR_OK;
}

static void get_cpu_metrics(sigar_t *sigar,
                            sigar_cpu_t *cpu, int32_t *cpu_time)
{
    cpu->user = SIGAR_TICK2SEC(cpu_time[CP_USER]);

    cpu->sys  = SIGAR_TICK2SEC(cpu_time[CP_SYS] +
                               cpu_time[CP_SSYS] +
                               cpu_time[CP_INTR]);

    cpu->nice = SIGAR_TICK2SEC(cpu_time[CP_NICE]);

    cpu->idle = SIGAR_TICK2SEC(cpu_time[CP_IDLE]);

    cpu->wait = SIGAR_TICK2SEC(cpu_time[CP_WAIT] +
                               cpu_time[CP_SWAIT] +
                               cpu_time[CP_BLOCK]);
    
    cpu->total =
        cpu->user + cpu->sys + cpu->nice + cpu->idle + cpu->wait;
}

int sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
    struct pst_dynamic stats;

    pstat_getdynamic(&stats, sizeof(stats), 1, 0);
    sigar->ncpu = stats.psd_proc_cnt;

    get_cpu_metrics(sigar, cpu, stats.psd_cpu_time);

    return SIGAR_OK;
}

int sigar_cpu_list_get(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    int i;
    struct pst_dynamic stats;

    pstat_getdynamic(&stats, sizeof(stats), 1, 0);
    sigar->ncpu = stats.psd_proc_cnt;

    sigar_cpu_list_create(cpulist);

    for (i=0; i<sigar->ncpu; i++) {
        sigar_cpu_t *cpu;
        struct pst_processor proc;

        if (pstat_getprocessor(&proc, sizeof(proc), 1, i) < 0) {
            continue;
        }

        SIGAR_CPU_LIST_GROW(cpulist);

        cpu = &cpulist->data[cpulist->number++];

        get_cpu_metrics(sigar, cpu, proc.psp_cpu_time);
    }

    return SIGAR_OK;
}

int sigar_uptime_get(sigar_t *sigar,
                     sigar_uptime_t *uptime)
{
    uptime->uptime = time(NULL) - sigar->pstatic.boot_time;

    return SIGAR_OK;
}

int sigar_loadavg_get(sigar_t *sigar,
                      sigar_loadavg_t *loadavg)
{
    struct pst_dynamic stats;

    pstat_getdynamic(&stats, sizeof(stats), 1, 0);

    loadavg->loadavg[0] = stats.psd_avg_1_min;
    loadavg->loadavg[1] = stats.psd_avg_5_min;
    loadavg->loadavg[2] = stats.psd_avg_15_min;
    
    return SIGAR_OK;
}

#define PROC_ELTS 16

int sigar_proc_list_get(sigar_t *sigar,
                        sigar_proc_list_t *proclist)
{
    int num, idx=0;
    struct pst_status proctab[PROC_ELTS];

    sigar_proc_list_create(proclist);

    while ((num = pstat_getproc(proctab, sizeof(proctab[0]),
                                PROC_ELTS, idx)) > 0)
    {
        int i;

        for (i=0; i<num; i++) {
            SIGAR_PROC_LIST_GROW(proclist);
            proclist->data[proclist->number++] =
                proctab[i].pst_pid;
        }

        idx = proctab[num-1].pst_idx + 1;
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

static int sigar_pstat_getproc(sigar_t *sigar, sigar_pid_t pid)
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

    if (pstat_getproc(sigar->pinfo,
                      sizeof(*sigar->pinfo),
                      0, pid) == -1)
    {
        return ESRCH;
    }

    return SIGAR_OK;
}

int sigar_proc_mem_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_mem_t *procmem)
{
    int pagesize = sigar->pstatic.page_size;
    int status = sigar_pstat_getproc(sigar, pid);
    struct pst_status *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    procmem->size = 
        pinfo->pst_tsize + /* text */
        pinfo->pst_dsize + /* data */
        pinfo->pst_ssize;  /* stack */

    procmem->size *= pagesize;
        
    procmem->vsize =
        pinfo->pst_vtsize + /* text */
        pinfo->pst_vdsize + /* data */
        pinfo->pst_vssize;  /* stack */

    procmem->vsize *= pagesize;
        
    procmem->rss = pinfo->pst_rssize * pagesize;

    procmem->resident = procmem->rss;

    procmem->share = pinfo->pst_shmsize * pagesize;

    procmem->minor_faults = pinfo->pst_minorfaults;
    procmem->major_faults = pinfo->pst_majorfaults;
    procmem->page_faults =
        procmem->minor_faults +
        procmem->major_faults;

    return SIGAR_OK;
}

int sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_cred_t *proccred)
{
    int status = sigar_pstat_getproc(sigar, pid);
    struct pst_status *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    proccred->uid  = pinfo->pst_uid;
    proccred->gid  = pinfo->pst_gid;
    proccred->euid = pinfo->pst_euid;
    proccred->egid = pinfo->pst_egid;

    return SIGAR_OK;
}

int sigar_proc_time_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_time_t *proctime)
{
    int status = sigar_pstat_getproc(sigar, pid);
    struct pst_status *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    proctime->start_time = pinfo->pst_start;
    proctime->start_time *= 1000;
    proctime->user = pinfo->pst_utime;
    proctime->sys  = pinfo->pst_stime;
    proctime->total = proctime->user + proctime->sys;

    return SIGAR_OK;
}

int sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_state_t *procstate)
{
    int status = sigar_pstat_getproc(sigar, pid);
    struct pst_status *pinfo = sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

        
    SIGAR_SSTRCPY(procstate->name, pinfo->pst_ucomm);
    procstate->ppid = pinfo->pst_ppid;
    procstate->tty  = makedev(pinfo->pst_term.psd_major,
                              pinfo->pst_term.psd_minor);
    procstate->priority = pinfo->pst_pri;
    procstate->nice     = pinfo->pst_nice;
    procstate->threads  = pinfo->pst_nlwps;
    procstate->processor = pinfo->pst_procnum;

    switch (pinfo->pst_stat) {
      case PS_SLEEP:
        procstate->state = 'S';
        break;
      case PS_RUN:
        procstate->state = 'R';
        break;
      case PS_STOP:
        procstate->state = 'T';
        break;
      case PS_ZOMBIE:
        procstate->state = 'Z';
        break;
      case PS_IDLE:
        procstate->state = 'D';
        break;
    }

    return SIGAR_OK;
}

/*
 * XXX: pst_cmd is only 64 chars of the command args.
 * according to HP forums there isn't a way to get them
 * all if > 64
 */
int sigar_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_args_t *procargs)
{
    char *args, *arg;
    struct pst_status status;

    if (pstat_getproc(&status, sizeof(status), 0, pid) == -1) {
        return ESRCH;
    }

    args = status.pst_cmd;
    sigar_proc_args_create(procargs);

    while (*args && (arg = sigar_getword(&args, ' '))) {
        SIGAR_PROC_ARGS_GROW(procargs);
        procargs->data[procargs->number++] = arg;
    }
    
    return SIGAR_OK;
}

int sigar_proc_env_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_env_t *procenv)
{
    return SIGAR_ENOTIMPL;
}

int sigar_proc_fd_get(sigar_t *sigar, sigar_pid_t pid,
                      sigar_proc_fd_t *procfd)
{
    struct pst_status status;
    int idx, i, n;
    struct pst_fileinfo psf[16];

    procfd->total = 0;

    if (pstat_getproc(&status, sizeof(status), 0, pid) == -1) {
        return ESRCH;
    }

    /* man pstat_getfile for index splaination */
    idx = (status.pst_idx << 16) | (0 & 0xffff);

    while ((n = pstat_getfile(psf, sizeof(psf[0]),
                              sizeof(psf)/sizeof(psf[0]),
                              idx)) > 0)
    {
        procfd->total += n;
        idx = psf[n-1].psf_idx + 1;
    }

    if (n == -1) {
        return errno;
    }

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

#include <mntent.h>

int sigar_os_fs_type_get(sigar_file_system_t *fsp)
{
    char *type = fsp->sys_type_name;

    switch (*type) {
      case 'h':
        if (strEQ(type, "hfs")) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
      case 'v':
        if (strEQ(type, "vxfs")) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
      case 'c':
        if (strEQ(type, "cdfs")) {
            fsp->type = SIGAR_FSTYPE_CDROM;
        }
        break;
    }

    return fsp->type;
}

int sigar_file_system_list_get(sigar_t *sigar,
                               sigar_file_system_list_t *fslist)
{
    struct mntent ent;
    char buf[1025];

    FILE *fp;
    sigar_file_system_t *fsp;

    if (!(fp = setmntent(MNT_CHECKLIST, "r"))) {
        return errno;
    }

    sigar_file_system_list_create(fslist);

    while (getmntent_r(fp, &ent, buf, sizeof(buf)) == 0) {
        if ((*(ent.mnt_type) == 's') &&
            strEQ(ent.mnt_type, "swap"))
        {
            /*
             * in this case, devname == "...", for
             * which statfs chokes on.  so skip it.
             * also notice hpux df command has no swap info.
             */
            continue;
        }
        
        SIGAR_FILE_SYSTEM_LIST_GROW(fslist);

        fsp = &fslist->data[fslist->number++];

        SIGAR_SSTRCPY(fsp->dir_name, ent.mnt_dir);
        SIGAR_SSTRCPY(fsp->dev_name, ent.mnt_fsname);
        SIGAR_SSTRCPY(fsp->sys_type_name, ent.mnt_type);
        sigar_fs_type_init(fsp);
    }

    endmntent(fp);

    return SIGAR_OK;
}

/* XXX this is exactly the same as linux and solaris is darn close */
#include <sys/vfs.h>

#define SIGAR_FS_BLOCKS_TO_BYTES(buf, f) \
    ((buf.f * (buf.f_bsize / 512)) >> 1)

#define FSDEV_ID(sb) (sb.st_ino + sb.st_dev)

static int create_fsdev_cache(sigar_t *sigar)
{
    sigar_file_system_list_t fslist;
    int i;
    int status =
        sigar_file_system_list_get(sigar, &fslist);

    if (status != SIGAR_OK) {
        return status;
    }

    sigar->fsdev = sigar_cache_new(15);

    for (i=0; i<fslist.number; i++) {
        sigar_file_system_t *fsp = &fslist.data[i];

        if (fsp->type == SIGAR_FSTYPE_LOCAL_DISK) {
            sigar_cache_entry_t *ent;
            struct stat sb;

            if (stat(fsp->dir_name, &sb) < 0) {
                continue;
            }

            ent = sigar_cache_get(sigar->fsdev, FSDEV_ID(sb));
            ent->value = strdup(fsp->dev_name);
        }
    }
}

int sigar_file_system_usage_get(sigar_t *sigar,
                                const char *dirname,
                                sigar_file_system_usage_t *fsusage)
{
    struct statfs buf;
    struct stat sb;

    if (statfs(dirname, &buf) != 0) {
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

    if (stat(dirname, &sb) == 0) {
        sigar_cache_entry_t *ent;
        struct pst_lvinfo lv;
        struct stat devsb;
        char *devname;
        int retval;

        ent = sigar_cache_get(sigar->fsdev, FSDEV_ID(sb));
        if (ent->value == NULL) {
            return SIGAR_OK;
        }

        if (stat((char *)ent->value, &devsb) < 0) {
            return SIGAR_OK;
        }

        retval = pstat_getlv(&lv, sizeof(lv), 0, (int)devsb.st_rdev);

        if (retval == 1) {
            fsusage->disk_reads  = lv.psl_rxfer;
            fsusage->disk_writes = lv.psl_wxfer;
            fsusage->disk_read_bytes  = lv.psl_rcount;
            fsusage->disk_write_bytes = lv.psl_wcount;
            fsusage->disk_queue       = SIGAR_FIELD_NOTIMPL;
        }
    }

    return SIGAR_OK;
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    int i;
    struct pst_dynamic stats;

    pstat_getdynamic(&stats, sizeof(stats), 1, 0);
    sigar->ncpu = stats.psd_proc_cnt;

    sigar_cpu_info_list_create(cpu_infos);

    for (i=0; i<sigar->ncpu; i++) {
        sigar_cpu_info_t *info;
        struct pst_processor proc;

        if (pstat_getprocessor(&proc, sizeof(proc), 1, i) < 0) {
            perror("pstat_getprocessor");
            continue;
        }

        SIGAR_CPU_INFO_LIST_GROW(cpu_infos);

        info = &cpu_infos->data[cpu_infos->number++];

        SIGAR_SSTRCPY(info->vendor, "HP"); /*XXX*/
        SIGAR_SSTRCPY(info->model, "PA RISC"); /*XXX*/
        info->mhz = sigar->ticks * proc.psp_iticksperclktick / 1000000;
        info->cache_size = SIGAR_FIELD_NOTIMPL; /*XXX*/
    }

    return SIGAR_OK;
}

static int sigar_get_mib_info(sigar_t *sigar,
                              struct nmparms *parms)
{
    if (sigar->mib < 0) {
        if ((sigar->mib = open_mib("/dev/ip", O_RDONLY, 0, 0)) < 0) {
            return errno;
        }
    }
    return get_mib_info(sigar->mib, parms);
}

int sigar_net_route_list_get(sigar_t *sigar,
                             sigar_net_route_list_t *routelist)
{
    int status, count, i;
    unsigned int len;
    struct nmparms parms;
    mib_ipRouteEnt *routes;
    sigar_net_route_t *route;

    len = sizeof(count);
    parms.objid = ID_ipRouteNumEnt;
    parms.buffer = &count;
    parms.len = &len;

    if ((status = sigar_get_mib_info(sigar, &parms)) != SIGAR_OK) {
        return status;
    }

    len = count * sizeof(*routes);
    routes = malloc(len);

    parms.objid = ID_ipRouteTable;
    parms.buffer = routes;
    parms.len = &len;

    if ((status = sigar_get_mib_info(sigar, &parms)) != SIGAR_OK) {
        free(routes);
        return status;
    }

    routelist->size = routelist->number = 0;

    sigar_net_route_list_create(routelist);

    for (i=0; i<count; i++) {
        mib_ipRouteEnt *ent = &routes[i];

        SIGAR_NET_ROUTE_LIST_GROW(routelist);

        route = &routelist->data[routelist->number++];
        SIGAR_ZERO(route); /* XXX: other fields */
        
        route->destination = ent->Dest;
        route->mask        = ent->Mask;
        route->gateway     = ent->NextHop;

        route->flags = SIGAR_RTF_UP;
        if ((route->destination == 0) &&
            (route->mask == 0))
        {
            route->flags |= SIGAR_RTF_GATEWAY;
        }
    }

    free(routes);
    
    return SIGAR_OK;
}

static int get_mib_ifstat(sigar_t *sigar,
                          const char *name,
                          mib_ifEntry *mib)
{
    int status, count, i;
    unsigned int len;
    nmapi_phystat *stat;
    struct nmparms parms;

    len = sizeof(count);
    parms.objid = ID_ifNumber;
    parms.buffer = &count;
    parms.len = &len;

    if ((status = sigar_get_mib_info(sigar, &parms)) != SIGAR_OK) {
        return status;
    }

    len = sizeof(nmapi_phystat) * count;

    if (sigar->ifconf_len < len) {
        sigar->ifconf_buf = realloc(sigar->ifconf_buf, len);
        sigar->ifconf_len = len;
    }

    if (get_physical_stat(sigar->ifconf_buf, &len) < 0) {
        return errno;
    }

    for (i=0, stat = (nmapi_phystat *)sigar->ifconf_buf;
         i<count;
         i++, stat++)
    {
        if (strEQ(stat->nm_device, name)) {
            memcpy(mib, &stat->if_entry, sizeof(*mib));
            return SIGAR_OK;
        }
    }

    return ENXIO;
}

int sigar_net_interface_stat_get(sigar_t *sigar, const char *name,
                                 sigar_net_interface_stat_t *ifstat)
{
    int status;
    mib_ifEntry mib;

    status = get_mib_ifstat(sigar, name, &mib);

    if (status != SIGAR_OK) {
        return status;
    }

    ifstat->rx_bytes    = mib.ifInOctets;
    ifstat->rx_packets  = mib.ifInUcastPkts + mib.ifInNUcastPkts;
    ifstat->rx_errors   = mib.ifInErrors;
    ifstat->rx_dropped  = mib.ifInDiscards;
    ifstat->rx_overruns = SIGAR_FIELD_NOTIMPL;
    ifstat->rx_frame    = SIGAR_FIELD_NOTIMPL;

    ifstat->tx_bytes      = mib.ifOutOctets;
    ifstat->tx_packets    = mib.ifOutUcastPkts + mib.ifOutNUcastPkts;
    ifstat->tx_errors     = mib.ifOutErrors;
    ifstat->tx_dropped    = mib.ifOutDiscards;
    ifstat->tx_overruns   = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_collisions = SIGAR_FIELD_NOTIMPL;
    ifstat->tx_carrier    = SIGAR_FIELD_NOTIMPL;

    return SIGAR_OK;
}

static int net_conn_get_udp_listen(sigar_t *sigar,
                                   sigar_net_connection_list_t *connlist,
                                   int flags)
{
    int status, count, i;
    unsigned int len;
    mib_udpLsnEnt *entries;
    struct nmparms parms;

    len = sizeof(count);
    parms.objid = ID_udpLsnNumEnt;
    parms.buffer = &count;
    parms.len = &len;

    if ((status = sigar_get_mib_info(sigar, &parms)) != SIGAR_OK) {
        return status;
    }

    if (count <= 0) {
        return ENOENT;
    }

    len =  count * sizeof(*entries);
    entries = malloc(len);
    parms.objid = ID_udpLsnTable;
    parms.buffer = entries;
    parms.len = &len;

    if ((status = sigar_get_mib_info(sigar, &parms)) != SIGAR_OK) {
        free(entries);
        return status;
    }

    for (i=0; i<count; i++) {
        sigar_net_connection_t *conn;
        mib_udpLsnEnt *entry = &entries[i];

        SIGAR_NET_CONNLIST_GROW(connlist);
        conn = &connlist->data[connlist->number++];

        conn->type = SIGAR_NETCONN_UDP;

        conn->local_port  = (unsigned short)entry->LocalPort;
        conn->remote_port = 0;

        sigar_inet_ntoa(sigar, entry->LocalAddress,
                        conn->local_address);

        SIGAR_SSTRCPY(conn->remote_address, "0.0.0.0");

        conn->send_queue = conn->receive_queue = SIGAR_FIELD_NOTIMPL;
    }

    free(entries);
    return SIGAR_OK;
}

static int net_conn_get_udp(sigar_t *sigar,
                            sigar_net_connection_list_t *connlist,
                            int flags)
{
    int status = SIGAR_OK;

    if (flags & SIGAR_NETCONN_SERVER) {
        status = net_conn_get_udp_listen(sigar, connlist, flags);
    }

    return status;
}

#define IS_TCP_SERVER(state, flags) \
    ((flags & SIGAR_NETCONN_SERVER) && (state == TCLISTEN))

#define IS_TCP_CLIENT(state, flags) \
    ((flags & SIGAR_NETCONN_CLIENT) && (state != TCLISTEN))

static int net_conn_get_tcp(sigar_t *sigar,
                            sigar_net_connection_list_t *connlist,
                            int flags)
{
    int status, count, i;
    unsigned int len;
    mib_tcpConnEnt *entries;
    struct nmparms parms;

    len = sizeof(count);
    parms.objid = ID_tcpConnNumEnt;
    parms.buffer = &count;
    parms.len = &len;

    if ((status = sigar_get_mib_info(sigar, &parms)) != SIGAR_OK) {
        return status;
    }

    if (count <= 0) {
        return ENOENT;
    }

    len =  count * sizeof(*entries);
    entries = malloc(len);
    parms.objid = ID_tcpConnTable;
    parms.buffer = entries;
    parms.len = &len;

    if ((status = sigar_get_mib_info(sigar, &parms)) != SIGAR_OK) {
        free(entries);
        return status;
    }

    for (i=0; i<count; i++) {
        sigar_net_connection_t *conn;
        mib_tcpConnEnt *entry = &entries[i];
        int state = entry->State;

        if (!(IS_TCP_SERVER(state, flags) ||
              IS_TCP_CLIENT(state, flags)))
        {
            continue;
        }

        SIGAR_NET_CONNLIST_GROW(connlist);
        conn = &connlist->data[connlist->number++];

        switch (state) {
          case TCCLOSED:
            conn->state = SIGAR_TCP_CLOSE;
            break;
          case TCLISTEN:
            conn->state = SIGAR_TCP_LISTEN;
            break;
          case TCSYNSENT:
            conn->state = SIGAR_TCP_SYN_SENT;
            break;
          case TCSYNRECEIVE:
            conn->state = SIGAR_TCP_SYN_RECV;
            break;
          case TCESTABLISED:
            conn->state = SIGAR_TCP_ESTABLISHED;
            break;
          case TCFINWAIT1:
            conn->state = SIGAR_TCP_FIN_WAIT1;
            break;
          case TCFINWAIT2:
            conn->state = SIGAR_TCP_FIN_WAIT2;
            break;
          case TCCLOSEWAIT:
            conn->state = SIGAR_TCP_CLOSE_WAIT;
            break;
          case TCCLOSING:
            conn->state = SIGAR_TCP_CLOSING;
            break;
          case TCLASTACK:
            conn->state = SIGAR_TCP_LAST_ACK;
            break;
          case TCTIMEWAIT:
            conn->state = SIGAR_TCP_TIME_WAIT;
            break;
          case TCDELETETCB:
          default:
            conn->state = SIGAR_TCP_UNKNOWN;
            break;
        }

        conn->local_port  = (unsigned short)entry->LocalPort;
        conn->remote_port = (unsigned short)entry->RemPort;
        conn->type = SIGAR_NETCONN_TCP;

        sigar_inet_ntoa(sigar, entry->LocalAddress,
                        conn->local_address);

        sigar_inet_ntoa(sigar, entry->RemAddress,
                        conn->remote_address);

        conn->send_queue = conn->receive_queue = SIGAR_FIELD_NOTIMPL;
    }

    free(entries);

    return SIGAR_OK;
}

int sigar_net_connection_list_get(sigar_t *sigar,
                                  sigar_net_connection_list_t *connlist,
                                  int flags)
{
    int status;

    sigar_net_connection_list_create(connlist);

    if (flags & SIGAR_NETCONN_TCP) {
        status = net_conn_get_tcp(sigar, connlist, flags);

        if (status != SIGAR_OK) {
            return status;
        }
    }

    if (flags & SIGAR_NETCONN_UDP) {
        status = net_conn_get_udp(sigar, connlist, flags);

        if (status != SIGAR_OK) {
            return status;
        }
    }

    return SIGAR_OK;
}

int sigar_proc_port_get(sigar_t *sigar, int protocol,
                        unsigned long port, sigar_pid_t *pid)
{
    return SIGAR_ENOTIMPL;
}
