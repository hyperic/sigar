#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/times.h>

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

#include <asm/page.h> /* for PAGE_SHIFT */

#define pageshift(x) ((x) << PAGE_SHIFT)

#define PROC_MEMINFO PROC_FS_ROOT "meminfo"
#define PROC_STAT    PROC_FS_ROOT "stat"
#define PROC_UPTIME  PROC_FS_ROOT "uptime"
#define PROC_LOADAVG PROC_FS_ROOT "loadavg"

#define PROC_PSTAT   "/stat"
#define PROC_PSTATUS "/status"

#define SYS_BLOCK "/sys/block"
#define PROC_PARTITIONS PROC_FS_ROOT "partitions"
#define PROC_DISKSTATS  PROC_FS_ROOT "diskstats"

/*
 * /proc/self/stat fields:
 * 1 - pid
 * 2 - comm
 * 3 - state
 * 4 - ppid
 * 5 - pgrp
 * 6 - session
 * 7 - tty_nr
 * 8 - tpgid
 * 9 - flags
 * 10 - minflt
 * 11 - cminflt
 * 12 - majflt
 * 13 - cmajflt
 * 14 - utime
 * 14 - stime
 * 15 - cutime
 * 16 - cstime
 * 17 - priority
 * 18 - nice
 * 19 - 0 (removed field)
 * 20 - itrealvalue
 * 21 - starttime
 * 22 - vsize
 * 23 - rss
 * 24 - rlim
 * 25 - startcode
 * 26 - endcode
 * 27 - startstack
 * 28 - kstkesp
 * 29 - kstkeip
 * 30 - signal
 * 31 - blocked
 * 32 - sigignore
 * 33 - sigcache
 * 34 - wchan
 * 35 - nswap
 * 36 - cnswap
 * 37 - exit_signal <-- looking for this.
 * 38 - processor
 * ... more for newer RH
 */

#define PROC_SIGNAL_IX 38

static int get_proc_signal_offset(void)
{
    char buffer[BUFSIZ], *ptr=buffer;
    int fields = 0;
    int status = sigar_file2str("/proc/self/stat",
                                buffer, sizeof(buffer));

    if (status != SIGAR_OK) {
        return 1;
    }

    while (*ptr) {
        if (*ptr++ == ' ') {
            fields++;
        }
    }

    return (fields - PROC_SIGNAL_IX) + 1;
}

sigar_pid_t sigar_pid_get(sigar_t *sigar)
{
    /* XXX cannot safely cache getpid unless using nptl */
    /* we can however, cache it for optimizations in the
     * case of proc_env_get for example.
     */
    sigar->pid = getpid();
    return sigar->pid;
}

int sigar_os_open(sigar_t **sigar)
{
    char buffer[BUFSIZ], *ptr;
    int status = sigar_file2str(PROC_STAT, buffer, sizeof(buffer));
    struct stat sb;

    *sigar = malloc(sizeof(**sigar));

    if (status != SIGAR_OK) {
        return status;
    }

    ptr = strstr(buffer, "\nbtime");
    ptr = sigar_skip_token(ptr);
    (*sigar)->boot_time = sigar_strtoul(ptr);

    (*sigar)->ticks = sysconf(_SC_CLK_TCK);

    (*sigar)->ram = -1;

    (*sigar)->proc_signal_offset = -1;

    (*sigar)->last_proc_stat.pid = -1;

    (*sigar)->ht_enabled = -1;

    if (stat(PROC_DISKSTATS, &sb) == 0) {
        (*sigar)->iostat = IOSTAT_DISKSTATS;
    }
    else if (stat(SYS_BLOCK, &sb) == 0) {
        (*sigar)->iostat = IOSTAT_SYS;
    }
    else if (stat(PROC_PARTITIONS, &sb) == 0) {
        /* XXX file exists does not mean is has the fields */
        (*sigar)->iostat = IOSTAT_PARTITIONS;
    }
    else {
        (*sigar)->iostat = IOSTAT_NONE;
    }

    (*sigar)->fsdev = NULL;

    return SIGAR_OK;
}

int sigar_os_close(sigar_t *sigar)
{
    if (sigar->fsdev) {
        sigar_cache_destroy(sigar->fsdev);
    }
    free(sigar);
    return SIGAR_OK;
}

char *sigar_os_error_string(sigar_t *sigar, int err)
{
    return NULL;
}

static int is_ht_enabled(sigar_t *sigar)
{
    if (sigar->ht_enabled == -1) {
        /* only check once */
        sigar_cpu_info_list_t cpuinfos;

        if (sigar_cpu_info_list_get(sigar, &cpuinfos) != SIGAR_OK) {
            sigar->ht_enabled = 0; /* chances we reach here: slim..none */
        }

        sigar_cpu_info_list_destroy(sigar, &cpuinfos);
    }

    return sigar->ht_enabled;
}

static int get_ram(sigar_t *sigar, sigar_mem_t *mem)
{
    char buffer[BUFSIZ], *ptr;
    FILE *fp;
    int total = 0;

    if (sigar->ram > 0) {
        /* return cached value */
        mem->ram = sigar->ram;
        return SIGAR_OK;
    }

    if (sigar->ram == 0) {
        return ENOENT;
    }

    /*
     * Memory Type Range Registers
     * write-back registers add up to the total.
     */
    if (!(fp = fopen("/proc/mtrr", "r"))) {
        return errno;
    }

    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        if (!(ptr = strstr(ptr, "size="))) {
            continue;
        }

        if (!strstr(ptr, "write-back")) {
            continue;
        }

        ptr += 5;
        while (sigar_isspace(*ptr)) {
            ++ptr;
        }

        total += atoi(ptr);
    }

    fclose(fp);

    if (total == 0) {
        return ENOENT;
    }

    mem->ram = sigar->ram = total;

    return SIGAR_OK;
}

#define MEMINFO_PARAM(a) a ":", SSTRLEN(a ":")

static SIGAR_INLINE sigar_uint64_t sigar_meminfo(char *buffer,
                                                 char *attr, int len)
{
    sigar_uint64_t val = 0;
    char *ptr, *tok;

    if ((ptr = strstr(buffer, attr))) {
        ptr += len;
        val = strtoull(ptr, &tok, 0);
        while (*tok == ' ') {
            ++tok;
        }
        if (*tok == 'k') {
            val *= 1024;
        }
        else if (*tok == 'M') {
            val *= (1024 * 1024);
        }
    }

    return val;
}

int sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
    sigar_uint64_t buffers, cached, kern;
    char buffer[BUFSIZ];

    int status = sigar_file2str(PROC_MEMINFO,
                                buffer, sizeof(buffer));

    if (status != SIGAR_OK) {
        return status;
    }

    mem->total  = sigar_meminfo(buffer, MEMINFO_PARAM("MemTotal"));
    mem->free   = sigar_meminfo(buffer, MEMINFO_PARAM("MemFree"));
    mem->used   = mem->total - mem->free;

    buffers = sigar_meminfo(buffer, MEMINFO_PARAM("Buffers"));
    cached  = sigar_meminfo(buffer, MEMINFO_PARAM("Cached"));

    kern = buffers + cached;
    mem->actual_free = mem->free + kern;
    mem->actual_used = mem->used - kern;

    mem->shared = SIGAR_FIELD_NOTIMPL; /* XXX where did this go in 2.6?? */

    if (get_ram(sigar, mem) != SIGAR_OK) {
        /* XXX other options on failure? */
        sigar_mem_calc_ram(sigar, mem);
    }

    return SIGAR_OK;
}

int sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
    char buffer[BUFSIZ];

    /* XXX: we open/parse the same file here as sigar_mem_get */
    int status = sigar_file2str(PROC_MEMINFO,
                                buffer, sizeof(buffer));

    if (status != SIGAR_OK) {
        return status;
    }

    swap->total  = sigar_meminfo(buffer, MEMINFO_PARAM("SwapTotal"));
    swap->free   = sigar_meminfo(buffer, MEMINFO_PARAM("SwapFree"));
    swap->used   = swap->total - swap->free;

    return SIGAR_OK;
}

static void get_cpu_metrics(sigar_t *sigar, sigar_cpu_t *cpu, char *line)
{
    char *ptr = sigar_skip_token(line); /* "cpu%d" */

    cpu->user += SIGAR_TICK2SEC(sigar_strtoul(ptr));
    cpu->nice += SIGAR_TICK2SEC(sigar_strtoul(ptr));
    cpu->sys  += SIGAR_TICK2SEC(sigar_strtoul(ptr));
    cpu->idle += SIGAR_TICK2SEC(sigar_strtoul(ptr));
    if (*ptr == ' ') {
        /* 2.6+ kernels only */
        cpu->wait += SIGAR_TICK2SEC(sigar_strtoul(ptr));
    }
    cpu->total += cpu->user + cpu->nice + cpu->sys + cpu->idle + cpu->wait;
}

int sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
    char buffer[BUFSIZ];
    int status = sigar_file2str(PROC_STAT, buffer, sizeof(buffer));

    if (status != SIGAR_OK) {
        return status;
    }

    SIGAR_ZERO(cpu);
    get_cpu_metrics(sigar, cpu, buffer);

    return SIGAR_OK;
}

int sigar_cpu_list_get(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    FILE *fp;
    char buffer[BUFSIZ], cpu_total[BUFSIZ], *ptr;
    int hthread = is_ht_enabled(sigar), i=0;
    sigar_cpu_t *cpu;

    if (!(fp = fopen(PROC_STAT, "r"))) {
        return errno;
    }

    /* skip first line */
    (void)fgets(cpu_total, sizeof(cpu_total), fp);

    sigar_cpu_list_create(cpulist);

    /* XXX: merge times of logical processors if hyperthreading */
    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        if (!strnEQ(ptr, "cpu", 3)) {
            break;
        }

        if (hthread && (i % sigar->lcpu)) {
            /* merge times of logical processors */
            cpu = &cpulist->data[cpulist->number-1];
        }
        else {
            SIGAR_CPU_LIST_GROW(cpulist);
            cpu = &cpulist->data[cpulist->number++];
            SIGAR_ZERO(cpu);
        }

        get_cpu_metrics(sigar, cpu, ptr);

        i++;
    }

    fclose(fp);

    if (cpulist->number == 0) {
        /* likely older kernel where cpu\d is not present */
        cpu = &cpulist->data[cpulist->number++];
        SIGAR_ZERO(cpu);
        get_cpu_metrics(sigar, cpu, cpu_total);
    }

    return SIGAR_OK;
}

int sigar_uptime_get(sigar_t *sigar,
                     sigar_uptime_t *uptime)
{
    char buffer[BUFSIZ], *ptr = buffer;
    int status = sigar_file2str(PROC_UPTIME, buffer, sizeof(buffer));

    if (status != SIGAR_OK) {
        return status;
    }

    uptime->uptime   = strtod(buffer, &ptr);

    return SIGAR_OK;
}

int sigar_loadavg_get(sigar_t *sigar,
                      sigar_loadavg_t *loadavg)
{
    char buffer[BUFSIZ], *ptr = buffer;
    int status = sigar_file2str(PROC_LOADAVG, buffer, sizeof(buffer));

    if (status != SIGAR_OK) {
        return status;
    }

    loadavg->loadavg[0] = strtod(buffer, &ptr);
    loadavg->loadavg[1] = strtod(ptr, &ptr);
    loadavg->loadavg[2] = strtod(ptr, &ptr);

    return SIGAR_OK;
}

/*
 * seems the easiest/fastest way to tell if a process listed in /proc
 * is a thread is to check the "exit signal" flag in /proc/num/stat.
 * any value other than SIGCHLD seems to be a thread.  this make hulk mad.
 * redhat's procps patch (named "threadbadhack.pat") does not use
 * this flag to filter out threads.  instead does much more expensive
 * comparisions.  their patch also bubbles up thread cpu times to the main
 * process.  functionality we currently lack.
 * when nptl is in use, this is not the case and all threads spawned from
 * a process have the same pid.  however, it seems both old-style linux
 * threads and nptl threads can be run on the same machine.
 * there is also the "Tgid" field in /proc/self/status which could be used
 * to detect threads, but this is not available in older kernels.
 */
static SIGAR_INLINE int proc_isthread(sigar_t *sigar, char *pidstr, int len)
{
    char buffer[BUFSIZ], *ptr=buffer;
    int fd, n, offset=sigar->proc_signal_offset;

    /* sprintf(buffer, "/proc/%s/stat", pidstr) */
    memcpy(ptr, PROCP_FS_ROOT, SSTRLEN(PROCP_FS_ROOT));
    ptr += SSTRLEN(PROCP_FS_ROOT);

    memcpy(ptr, pidstr, len);
    ptr += len;

    memcpy(ptr, PROC_PSTAT, SSTRLEN(PROC_PSTAT));
    ptr += SSTRLEN(PROC_PSTAT);

    *ptr = '\0';

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        /* unlikely if pid was from readdir proc */
        return 0;
    }

    n = read(fd, buffer, sizeof(buffer));
    close(fd);

    if (n < 0) {
        return 0; /* chances: slim..none */
    }

    buffer[n--] = '\0';

    /* exit_signal is the second to last field so we look backwards.
     * XXX if newer kernels drop more turds in this file we'll need
     * to go the other way.  luckily linux has no real api for this shit.
     */

    /* skip trailing crap */
    while ((n > 0) && !isdigit(buffer[n--])) ;

    while (offset-- > 0) {
        /* skip last field */
        while ((n > 0) && isdigit(buffer[n--])) ;

        /* skip whitespace */
        while ((n > 0) && !isdigit(buffer[n--])) ;
    }

    if (n < 3) {
        return 0; /* hulk smashed /proc? */
    }

    ptr = &buffer[n];
    /*
     * '17' == SIGCHLD == real process.
     * '33' and '0' are threads
     */
    if ((*ptr++ == '1') &&
        (*ptr++ == '7') &&
        (*ptr++ == ' '))
    {
        return 0;
    }

    return 1;
}

int sigar_proc_list_get(sigar_t *sigar,
                        sigar_proc_list_t *proclist)
{
    DIR *dirp = opendir(PROCP_FS_ROOT);
    struct dirent *ent, dbuf;

    if (!dirp) {
        return errno;
    }

    if (sigar->proc_signal_offset == -1) {
        sigar->proc_signal_offset = get_proc_signal_offset();
    }

    sigar_proc_list_create(proclist);

    while (readdir_r(dirp, &dbuf, &ent) == 0) {
        if (!ent) {
            break;
        }

        if (!sigar_isdigit(*ent->d_name)) {
            continue;
        }

        if (proc_isthread(sigar, ent->d_name, strlen(ent->d_name)))
        {
            continue;
        }

        /* XXX: more sanity checking */

        SIGAR_PROC_LIST_GROW(proclist);

        proclist->data[proclist->number++] =
            strtoul(ent->d_name, NULL, 10);
    }

    closedir(dirp);

    return SIGAR_OK;
}

int sigar_proc_stat_get(sigar_t *sigar,
                        sigar_proc_stat_t *procstat)
{
    int status = /* XXX optimize */
        sigar_proc_count(sigar, &procstat->total);

    return status;
}

static int proc_stat_read(sigar_t *sigar, sigar_pid_t pid)
{
    char buffer[BUFSIZ], *ptr=buffer, *tmp;
    unsigned int len;
    linux_proc_stat_t *pstat = &sigar->last_proc_stat;
    int status;

    time_t timenow = time(NULL);

    /* 
     * short-lived cache read/parse of last /proc/pid/stat
     * as this info is spread out across a few functions.
     */
    if (pstat->pid == pid) {
        if ((timenow - pstat->mtime) < SIGAR_LAST_PROC_EXPIRE) {
            return SIGAR_OK;
        }
    }

    pstat->pid = pid;
    pstat->mtime = timenow;

    status = SIGAR_PROC_FILE2STR(buffer, pid, PROC_PSTAT);

    if (status != SIGAR_OK) {
        return status;
    }

    ptr = strchr(ptr, '(')+1;

    tmp = strrchr(ptr, ')');
    len = tmp-ptr;

    if (len >= sizeof(pstat->name)) {
        len = sizeof(pstat->name)-1;
    }

    memcpy(pstat->name, ptr, len);
    pstat->name[len] = '\0';
    ptr = tmp+1;

    SIGAR_SKIP_SPACE(ptr);
    pstat->state = *ptr++;
    SIGAR_SKIP_SPACE(ptr);

    pstat->ppid = sigar_strtoul(ptr);
    ptr = sigar_skip_token(ptr); /* pgrp */
    ptr = sigar_skip_token(ptr); /* session */
    pstat->tty = sigar_strtoul(ptr);
    ptr = sigar_skip_token(ptr); /* tty pgrp */

    ptr = sigar_skip_token(ptr); /* flags */
    pstat->minor_faults = sigar_strtoul(ptr);
    ptr = sigar_skip_token(ptr); /* cmin flt */
    pstat->major_faults = sigar_strtoul(ptr);
    ptr = sigar_skip_token(ptr); /* cmaj flt */

    pstat->utime = sigar_strtoul(ptr) / sigar->ticks;
    pstat->stime = sigar_strtoul(ptr) / sigar->ticks;

    ptr = sigar_skip_token(ptr); /* cutime */
    ptr = sigar_skip_token(ptr); /* cstime */

    pstat->priority = sigar_strtoul(ptr);
    pstat->nice     = sigar_strtoul(ptr);

    ptr = sigar_skip_token(ptr); /* timeout */
    ptr = sigar_skip_token(ptr); /* it_real_value */

    pstat->start_time  = sigar_strtoul(ptr);
    pstat->start_time /= sigar->ticks;
    pstat->start_time += sigar->boot_time; /* seconds */
    pstat->start_time *= 1000; /* milliseconds */

    pstat->vsize = sigar_strtoul(ptr);
    pstat->rss   = pageshift(sigar_strtoul(ptr));

    ptr = sigar_skip_token(ptr); /* startcode */
    ptr = sigar_skip_token(ptr); /* endcode */
    ptr = sigar_skip_token(ptr); /* startstack */
    ptr = sigar_skip_token(ptr); /* kstkesp */
    ptr = sigar_skip_token(ptr); /* kstkeip */
    ptr = sigar_skip_token(ptr); /* signal */
    ptr = sigar_skip_token(ptr); /* blocked */
    ptr = sigar_skip_token(ptr); /* sigignore */
    ptr = sigar_skip_token(ptr); /* sigcache */
    ptr = sigar_skip_token(ptr); /* wchan */
    ptr = sigar_skip_token(ptr); /* nswap */
    ptr = sigar_skip_token(ptr); /* cnswap */
    ptr = sigar_skip_token(ptr); /* exit_signal */

    ptr = sigar_skip_token(ptr);
    pstat->processor = sigar_strtoul(ptr);

    return SIGAR_OK;
}

int sigar_proc_mem_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_mem_t *procmem)
{
    char buffer[BUFSIZ], *ptr=buffer;
    int status = proc_stat_read(sigar, pid);
    linux_proc_stat_t *pstat = &sigar->last_proc_stat;

    procmem->vsize = pstat->vsize;
    procmem->rss   = pstat->rss;
    procmem->minor_faults = pstat->minor_faults;
    procmem->major_faults = pstat->major_faults;
    procmem->page_faults =
        procmem->minor_faults + procmem->major_faults;
    
    status = SIGAR_PROC_FILE2STR(buffer, pid, "/statm");

    if (status != SIGAR_OK) {
        return status;
    }

    procmem->size     = pageshift(sigar_strtoul(ptr));
    procmem->resident = pageshift(sigar_strtoul(ptr));
    procmem->share    = pageshift(sigar_strtoul(ptr));

    return SIGAR_OK;
}

int sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_cred_t *proccred)
{
    char buffer[BUFSIZ], *ptr;
    int status = SIGAR_PROC_FILE2STR(buffer, pid, PROC_PSTATUS);

    if (status != SIGAR_OK) {
        return status;
    }

    ptr = strstr(buffer, "\nUid:");
    ptr = sigar_skip_token(ptr);

    proccred->uid  = sigar_strtoul(ptr);
    proccred->euid = sigar_strtoul(ptr);

    ptr = strstr(ptr, "\nGid:");
    ptr = sigar_skip_token(ptr);

    proccred->gid  = sigar_strtoul(ptr);
    proccred->egid = sigar_strtoul(ptr);

    return SIGAR_OK;
}

int sigar_proc_time_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_time_t *proctime)
{
    int status = proc_stat_read(sigar, pid);
    linux_proc_stat_t *pstat = &sigar->last_proc_stat;

    if (status != SIGAR_OK) {
        return status;
    }

    proctime->user = pstat->utime;
    proctime->sys  = pstat->stime;
    proctime->total = proctime->user + proctime->sys;
    proctime->start_time = pstat->start_time;

    return SIGAR_OK;
}

static int proc_status_get(sigar_t *sigar, sigar_pid_t pid,
                           sigar_proc_state_t *procstate)
{
    char buffer[BUFSIZ], *ptr;
    int status = SIGAR_PROC_FILE2STR(buffer, pid, PROC_PSTATUS);

    if (status != SIGAR_OK) {
        return status;
    }

    ptr = strstr(buffer, "\nThreads:");
    if (ptr) {
        /* 2.6+ kernel only */
        ptr = sigar_skip_token(ptr);
        procstate->threads = sigar_strtoul(ptr);
    }
    else {
        procstate->threads = SIGAR_FIELD_NOTIMPL;
    }

    return SIGAR_OK;
}

int sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_state_t *procstate)
{
    int status = proc_stat_read(sigar, pid);
    linux_proc_stat_t *pstat = &sigar->last_proc_stat;

    if (status != SIGAR_OK) {
        return status;
    }

    memcpy(procstate->name, pstat->name, sizeof(procstate->name));
    procstate->state = pstat->state;

    procstate->ppid     = pstat->ppid;
    procstate->tty      = pstat->tty;
    procstate->priority = pstat->priority;
    procstate->nice     = pstat->nice;
    procstate->processor = pstat->processor;

    if (is_ht_enabled(sigar)) {
        procstate->processor /= sigar->lcpu;
    }

    proc_status_get(sigar, pid, procstate);

    return SIGAR_OK;
}

int sigar_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                        sigar_proc_args_t *procargs)
{
    return sigar_procfs_args_get(sigar, pid, procargs);
}

int sigar_proc_env_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_env_t *procenv)
{
    int fd;
    char buffer[ARG_MAX]; /* XXX: ARG_MAX == 130k */
    char name[BUFSIZ];
    size_t len;
    char *ptr, *end;

    /* optimize if pid == $$ and type == ENV_KEY */
    SIGAR_PROC_ENV_KEY_LOOKUP();

    (void)SIGAR_PROC_FILENAME(name, pid, "/environ");

    if ((fd = open(name, O_RDONLY)) < 0) {
        if (errno == ENOENT) {
            return ESRCH;
        }
        return errno;
    }

    len = read(fd, buffer, sizeof(buffer));

    close(fd);

    buffer[len] = '\0';
    ptr = buffer;

    end = buffer + len;
    while (ptr < end) {
        char *val = strchr(ptr, '=');
        int klen, vlen, status;
        char key[128]; /* XXX is there a max key size? */

        if (val == NULL) {
            /* not key=val format */
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
    int status =
        sigar_proc_fd_count(sigar, pid, &procfd->total);

    return status;
}

int sigar_proc_exe_get(sigar_t *sigar, sigar_pid_t pid,
                       sigar_proc_exe_t *procexe)
{
    int len;
    char name[BUFSIZ];

    (void)SIGAR_PROC_FILENAME(name, pid, "/cwd");

    if ((len = readlink(name, procexe->cwd,
                        sizeof(procexe->cwd)-1)) < 0)
    {
        return errno;
    }

    procexe->cwd[len] = '\0';

    (void)SIGAR_PROC_FILENAME(name, pid, "/exe");

    if ((len = readlink(name, procexe->name,
                        sizeof(procexe->name)-1)) < 0)
    {
        return errno;
    }

    procexe->name[len] = '\0';

    (void)SIGAR_PROC_FILENAME(name, pid, "/root");

    if ((len = readlink(name, procexe->root,
                        sizeof(procexe->root)-1)) < 0)
    {
        return errno;
    }

    procexe->root[len] = '\0';

    return SIGAR_OK;
}

int sigar_proc_modules_get(sigar_t *sigar, sigar_pid_t pid,
                           sigar_proc_modules_t *procmods)
{
    FILE *fp;
    char buffer[BUFSIZ], *ptr;
    unsigned long inode, last_inode = 0;

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/maps");

    if (!(fp = fopen(buffer, "r"))) {
        return errno;
    }

    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        int len, status;
        /* skip region, flags, offset, dev */
        ptr = sigar_skip_multiple_token(ptr, 4);
        inode = sigar_strtoul(ptr);

        if ((inode == 0) || (inode == last_inode)) {
            last_inode = 0;
            continue;
        }

        last_inode = inode;
        SIGAR_SKIP_SPACE(ptr);
        len = strlen(ptr);
        ptr[len-1] = '\0'; /* chop \n */

        status =
            procmods->module_getter(procmods->data,
                                    ptr, len-1);

        if (status != SIGAR_OK) {
            /* not an error; just stop iterating */
            break;
        }
    }
    
    fclose(fp);

    return SIGAR_OK;
}

#define TIME_NSEC(t) \
    SIGAR_SEC2NANO(SIGAR_TICK2SEC(((sigar_uint64_t)(t))))

int sigar_thread_cpu_get(sigar_t *sigar,
                         sigar_uint64_t id,
                         sigar_thread_cpu_t *cpu)
{
    struct tms now;

    if (id != 0) {
        return SIGAR_ENOTIMPL;
    }

    times(&now);

    cpu->user  = TIME_NSEC(now.tms_utime);
    cpu->sys   = TIME_NSEC(now.tms_stime);
    cpu->total = TIME_NSEC(now.tms_utime + now.tms_stime);

    return SIGAR_OK;
}

#include <mntent.h>
#include <sys/statfs.h>

int sigar_os_fs_type_get(sigar_file_system_t *fsp)
{
    char *type = fsp->sys_type_name;

    switch (*type) {
      case 'e':
        if (strnEQ(type, "ext", 3)) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
      case 'h':
        if (strEQ(type, "hpfs")) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
      case 'r':
        if (strEQ(type, "reiserfs")) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
      case 'x':
        if (strEQ(type, "xfs") || strEQ(type, "xiafs")) {
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
        }
        break;
    }

    return fsp->type;
}

int sigar_file_system_list_get(sigar_t *sigar,
                               sigar_file_system_list_t *fslist)
{
    struct mntent ent;
    char buf[1025]; /* buffer for strings within ent */
    FILE *fp;
    sigar_file_system_t *fsp;

    if (!(fp = setmntent(MOUNTED, "r"))) {
        return errno;
    }

    sigar_file_system_list_create(fslist);

    while (getmntent_r(fp, &ent, buf, sizeof(buf))) {
        SIGAR_FILE_SYSTEM_LIST_GROW(fslist);

        fsp = &fslist->data[fslist->number++];

        fsp->type = SIGAR_FSTYPE_UNKNOWN; /* unknown, will be set later */
        SIGAR_SSTRCPY(fsp->dir_name, ent.mnt_dir);
        SIGAR_SSTRCPY(fsp->dev_name, ent.mnt_fsname);
        SIGAR_SSTRCPY(fsp->sys_type_name, ent.mnt_type);
        sigar_fs_type_get(fsp);
    }

    endmntent(fp);

    return SIGAR_OK;
}

#define FSDEV_NONE "__NONE__"

#define FSDEV_ID(sb) (sb.st_ino + sb.st_dev)

static void fsdev_free(void *ptr)
{
    if (ptr != FSDEV_NONE) {
        free(ptr);
    }
}

static char *get_fsdev(sigar_t *sigar,
                       const char *dirname,
                       char *fsdev)
{
    sigar_cache_entry_t *entry;
    struct stat sb;
    sigar_uint64_t id;

    if (stat(dirname, &sb) < 0) {
        return NULL;
    }

    id = FSDEV_ID(sb);

    if (!sigar->fsdev) {
        sigar->fsdev = sigar_cache_new(15);
        sigar->fsdev->free_value = fsdev_free;
    }

    entry = sigar_cache_get(sigar->fsdev, id);

    if (entry->value == NULL) {
        sigar_file_system_list_t fslist;
        int status = sigar_file_system_list_get(sigar, &fslist);
        int i;

        if (status != SIGAR_OK) {
            return NULL;
        }

        for (i=0; i<fslist.number; i++) {
            sigar_file_system_t *fsp = &fslist.data[i];

            if (fsp->type == SIGAR_FSTYPE_LOCAL_DISK) {
                int retval = stat(fsp->dir_name, &sb);
                sigar_cache_entry_t *ent;
                char *ptr;

                if (retval < 0) {
                    return NULL; /* cant cache w/o inode */
                }

                ent = sigar_cache_get(sigar->fsdev, FSDEV_ID(sb));
                if (ent->value) {
                    continue; /* already cached */
                }

                ptr = fsp->dev_name;
                if (strnEQ(ptr, "/dev/", 5)) {
                    ptr += 5;
                    ent->value = sigar_strdup(ptr);
                    continue;
                }

                ent->value = FSDEV_NONE;
            }
        }

        sigar_file_system_list_destroy(sigar, &fslist);
    }

    if (entry->value == FSDEV_NONE) {
        return NULL;
    }
    else if (entry->value == NULL) {
        entry->value = FSDEV_NONE;
        return NULL;
    }
    else {
        strcpy(fsdev, (char*)entry->value);
        return fsdev;
    }
}

static int get_iostat_sys(sigar_t *sigar,
                          const char *dirname,
                          sigar_file_system_usage_t *fsusage)
{
    char stat[1025], dev[1025];
    char *name, *ptr, *fsdev;
    int partition, status;

    name = fsdev = get_fsdev(sigar, dirname, dev);

    if (!name) {
        return ENOENT;
    }

    while (!sigar_isdigit(*fsdev)) {
        fsdev++;
    }

    partition = strtoul(fsdev, NULL, 0);
    *fsdev = '\0';

    sprintf(stat, SYS_BLOCK "/%s/%s%d/stat", name, name, partition);

    status = sigar_file2str(stat, dev, sizeof(dev));
    if (status != SIGAR_OK) {
        return status;
    }

    ptr = dev;
    ptr = sigar_skip_token(ptr);
    fsusage->disk_reads = sigar_strtoul(ptr);
    ptr = sigar_skip_token(ptr);
    fsusage->disk_writes = sigar_strtoul(ptr);

    fsusage->disk_read_bytes  = SIGAR_FIELD_NOTIMPL;
    fsusage->disk_write_bytes = SIGAR_FIELD_NOTIMPL;
    fsusage->disk_queue       = SIGAR_FIELD_NOTIMPL;

    return SIGAR_OK;
}

static int get_iostat_proc_dstat(sigar_t *sigar,
                                 const char *dirname,
                                 sigar_file_system_usage_t *fsusage)
{
    FILE *fp;
    char buffer[1025], dev[1025];
    char *name, *ptr;
    int len;

    name = get_fsdev(sigar, dirname, dev);

    if (!name) {
        return ENOENT;
    }

    len = strlen(name);

    if (!(fp = fopen(PROC_DISKSTATS, "r"))) {
        return errno;
    }

    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        /* major, minor */
        ptr = sigar_skip_multiple_token(ptr, 2);        
        SIGAR_SKIP_SPACE(ptr);

        if (strnEQ(ptr, name, len)) {
            int num, status=SIGAR_OK;
            unsigned long
                rio, rmerge, rsect, ruse,
                wio, wmerge, wsect, wuse,
                running, use, aveq;

            ptr += len; /* name */
            SIGAR_SKIP_SPACE(ptr);

            num = sscanf(ptr,
                         "%lu %lu %lu %lu "
                         "%lu %lu %lu %lu "
                         "%lu %lu %lu",
                         &rio, &rmerge, &rsect, &ruse,
                         &wio, &wmerge, &wsect, &wuse,
                         &running, &use, &aveq);

            if (num == 11) {
                fsusage->disk_queue  = aveq / 1000;
            }
            else if (num == 4) {
                wio = rsect;
                rsect = rmerge;
                wsect = ruse;
                fsusage->disk_queue = SIGAR_FIELD_NOTIMPL;
            }
            else {
                status = ENOENT;
            }

            fsusage->disk_reads = rio;
            fsusage->disk_writes = wio;
            fsusage->disk_read_bytes  = rsect;
            fsusage->disk_write_bytes = wsect;

            fclose(fp);
            return status;
        }
    }

    fclose(fp);

    return ENOENT;
}

static int get_iostat_procp(sigar_t *sigar,
                            const char *dirname,
                            sigar_file_system_usage_t *fsusage)
{
    FILE *fp;
    char buffer[1025], dev[1025];
    char *name, *ptr;
    int len;

    name = get_fsdev(sigar, dirname, dev);

    if (!name) {
        return ENOENT;
    }

    len = strlen(name);

    if (!(fp = fopen(PROC_PARTITIONS, "r"))) {
        return errno;
    }

    (void)fgets(buffer, sizeof(buffer), fp); /* skip header */
    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        /* major, minor, #blocks */
        ptr = sigar_skip_multiple_token(ptr, 3);
        SIGAR_SKIP_SPACE(ptr);

        if (strnEQ(ptr, name, len)) {
            ptr = sigar_skip_token(ptr); /* name */
            fsusage->disk_reads = sigar_strtoul(ptr); /* rio */
            ptr = sigar_skip_token(ptr);  /* rmerge */ 
            fsusage->disk_read_bytes  = sigar_strtoul(ptr); /* rsect */
            ptr = sigar_skip_token(ptr);  /* ruse */ 

            ptr = sigar_skip_token(ptr);  /* wmerge */ 
            fsusage->disk_write_bytes = sigar_strtoul(ptr); /* wsect */
            fsusage->disk_writes = sigar_strtoul(ptr); /* wio */
            /* wuse, running, use */
            ptr = sigar_skip_multiple_token(ptr, 3);
            fsusage->disk_queue       = sigar_strtoul(ptr); /* aveq */
            fsusage->disk_queue /= 1000;

            fclose(fp);
            return SIGAR_OK;
        }
    }

    fclose(fp);

    return ENOENT;
}

#include <sys/vfs.h>

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
    fsusage->avail = SIGAR_FS_BLOCKS_TO_BYTES(buf, f_bavail);
    fsusage->used  = fsusage->total - fsusage->free;
    fsusage->files = buf.f_files;
    fsusage->free_files = buf.f_ffree;
    fsusage->use_percent = sigar_file_system_usage_calc_used(sigar, fsusage);

    /*
     * 2.2 has metrics /proc/stat, but wtf is the device mapping?
     * 2.4 has /proc/partitions w/ the metrics.
     * 2.6 has /proc/partitions w/o the metrics.
     *     instead the metrics are within the /proc-like /sys filesystem.
     *     also has /proc/diskstats
     */
    switch (sigar->iostat) {
      case IOSTAT_SYS:
        if (get_iostat_sys(sigar, dirname, fsusage) == SIGAR_OK) {
            return SIGAR_OK;
        }
        break;
      case IOSTAT_DISKSTATS:
        if (get_iostat_proc_dstat(sigar, dirname, fsusage) == SIGAR_OK) {
            return SIGAR_OK;
        }
        break;
      case IOSTAT_PARTITIONS:
        if (get_iostat_procp(sigar, dirname, fsusage) == SIGAR_OK) {
            return SIGAR_OK;
        }
        break;
      /*
       * case IOSTAT_SOME_OTHER_WIERD_THING:
       * break;
       */
      case IOSTAT_NONE:
        break;
    }

    SIGAR_DISK_STATS_NOTIMPL(fsusage);

    return SIGAR_OK;
}

static SIGAR_INLINE char *cpu_info_strval(char *ptr)
{
    if ((ptr = strchr(ptr, ':'))) {
        ptr++;
        while (isspace (*ptr)) ptr++;
        return ptr;
    }
    return NULL;
}

static SIGAR_INLINE void cpu_info_strcpy(char *ptr, char *buf, int len)
{
    int slen;
    ptr = cpu_info_strval(ptr);
    if (!ptr) {
        return;
    }
    slen = strlen(ptr);
    strncpy(buf, ptr, len);
    buf[len] = '\0';
    if (slen < len) {
        buf[slen-1] = '\0'; /* rid \n */
    }
}

static int get_cpu_info(sigar_t *sigar, sigar_cpu_info_t *info,
                        FILE *fp, int *id)
{
    char buffer[BUFSIZ], *ptr;

    int found = 0, siblings = 0;

    *id = -1;

    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        switch (*ptr) {
          case 'p': /* processor	: 0 */
            if (strnEQ(ptr, "processor", 9)) {
                found = 1;
            }
            else if (strnEQ(ptr, "physical id", 11)) {
                ptr = cpu_info_strval(ptr);
                *id = atoi(ptr);
            }
            break;
          case 'v':
            if (strnEQ(ptr, "vendor_id", 9)) {
                cpu_info_strcpy(ptr, info->vendor, sizeof(info->vendor));
                if (strEQ(info->vendor, "GenuineIntel")) {
                    SIGAR_SSTRCPY(info->vendor, "Intel");
                }
                else if (strEQ(info->vendor, "AuthenticAMD")) {
                    SIGAR_SSTRCPY(info->vendor, "AMD");
                }
            }
            break;
          case 'm':
            if (strnEQ(ptr, "model name", 10)) {
                cpu_info_strcpy(ptr, info->model, sizeof(info->model));
                sigar_cpu_model_adjust(sigar, info);
            }
            break;
          case 'c':
            if (strnEQ(ptr, "cpu MHz", 7)) {
                ptr = cpu_info_strval(ptr);
                info->mhz = atoi(ptr);
            }
            else if (strnEQ(ptr, "cache size", 10)) {
                ptr = cpu_info_strval(ptr);
                info->cache_size = sigar_strtoul(ptr);
            }
            break;
          case 's':
            /* this is horseshit.  why doesn't linux have a real api
             * like every other operation system.  if siblings == 1
             * then hyperthreading is disabled, so we shouldn't fold
             * the dups based on physical id attribute.
             */
            if (strnEQ(ptr, "siblings", 8)) {
                ptr = cpu_info_strval(ptr);
                siblings = atoi(ptr);
                if (siblings == 1) {
                    *id = -1;
                }
            }
            break;
          case 't':
            /* same as siblings, renamed in new kernels */
            if (strnEQ(ptr, "threads", 7)) {
                ptr = cpu_info_strval(ptr);
                siblings = atoi(ptr);
                if (siblings == 1) {
                    *id = -1;
                }
            }
            break;
           /* lone \n means end of info for this processor */
          case '\n':
            return found;
        }
    }

    return found;
}

int sigar_cpu_info_list_get(sigar_t *sigar,
                            sigar_cpu_info_list_t *cpu_infos)
{
    FILE *fp;
    int id, fake_id = -1;
    int cpu_id[36], cpu_ix=0;
    /* in the event that a box has > 36 cpus */
    int cpu_id_max = sizeof(cpu_id)/sizeof(int);

    if (!(fp = fopen(PROC_FS_ROOT "cpuinfo", "r"))) {
        return errno;
    }

    sigar->ht_enabled = 0; /* figure out below */

    sigar_cpu_info_list_create(cpu_infos);
    memset(&cpu_id[0], -1, sizeof(cpu_id));

    while (get_cpu_info(sigar, &cpu_infos->data[cpu_infos->number], fp, &id)) {
        fake_id++;

        if (id >= 0) {
            int i, fold=0;
            for (i=0; (i<cpu_ix) && (i<cpu_id_max); i++) {
                if (cpu_id[i] == id) {
                    fold = 1;
                    break;
                }
            }

            /* e.g. each Intel Xeon cpu is reported twice */
            /* fold dups based on physical id */
            if (fold) {
                sigar->ht_enabled = 1;
                sigar->lcpu = 2; /* XXX assume 2 for now */
                continue;
            }
            else {
                if (cpu_ix < cpu_id_max) {
                    cpu_id[cpu_ix++] = id;
                }
            }
        }
        else if (strEQ(cpu_infos->data[cpu_infos->number].model, "Xeon")) {
            /* Redhat AS 2.1 /proc/cpuinfo does not have any of the
             * attributes we use to detect hyperthreading, in this case
             * if model Xeon, assume HT enabled.  FUCK IT.
             */
            if (fake_id % 2) {
                sigar->ht_enabled = 1;
                sigar->lcpu = 2; /* XXX assume 2 for now */
                continue;
            }
        }

        ++cpu_infos->number;
        SIGAR_CPU_INFO_LIST_GROW(cpu_infos);
    }

    fclose(fp);

    return SIGAR_OK;
}

static unsigned int hex2int(const char *x)
{
    int i, ch;
    unsigned int j;

    for (i=0, j=0; i<8; i++) {
        ch = x[i];
        j <<= 4;
        if (isdigit(ch)) {
            j |= ch - '0';
        }
        else if (isupper(ch)) {
            j |= ch - ('A' - 10);
        }
        else {
            j |= ch - ('a' - 10);
        }
    }

    return j;
}

#ifdef __LP64__
#define ROUTE_FMT "%16s %128s %128s %X %ld %ld %ld %128s %ld %ld %ld\n"
#else
#define ROUTE_FMT "%16s %128s %128s %X %lld %lld %lld %128s %lld %lld %lld\n"
#endif
#define RTF_UP 0x0001

int sigar_net_route_list_get(sigar_t *sigar,
                             sigar_net_route_list_t *routelist)
{
    FILE *fp;
    char buffer[1024];
    char net_addr[128], gate_addr[128], mask_addr[128];
    int flags;
    sigar_net_route_t *route;

    routelist->size = routelist->number = 0;

    if (!(fp = fopen(PROC_FS_ROOT "net/route", "r"))) {
        return errno;
    }

    sigar_net_route_list_create(routelist);

    (void)fgets(buffer, sizeof(buffer), fp); /* skip header */
    while (fgets(buffer, sizeof(buffer), fp)) {
        int num;

        SIGAR_NET_ROUTE_LIST_GROW(routelist);
        route = &routelist->data[routelist->number++];

        /* XXX rid sscanf */
        num = sscanf(buffer, ROUTE_FMT,
                     route->ifname, net_addr, gate_addr,
                     &flags, &route->refcnt, &route->use,
                     &route->metric, mask_addr,
                     &route->mtu, &route->window, &route->irtt);

        if ((num < 10) || !(flags & RTF_UP)) {
            --routelist->number;
            continue;
        }

        route->flags = flags;
        route->destination = hex2int(net_addr);
        route->gateway = hex2int(gate_addr);
        route->mask = hex2int(mask_addr);
    }

    fclose(fp);

    return SIGAR_OK;
}

int sigar_net_interface_stat_get(sigar_t *sigar, const char *name,
                                 sigar_net_interface_stat_t *ifstat)
{
    int found = 0;
    char buffer[BUFSIZ];
    FILE *fp = fopen(PROC_FS_ROOT "net/dev", "r");
    
    if (!fp) {
        return errno;
    }

    /* skip header */
    fgets(buffer, sizeof(buffer), fp);
    fgets(buffer, sizeof(buffer), fp);

    while (fgets(buffer, sizeof(buffer), fp)) {
        char *ptr, *dev;

        dev = buffer;
        while (isspace(*dev)) {
            dev++;
        }

        if (!(ptr = strchr(dev, ':'))) {
            continue;
        }

        *ptr++ = 0;

        if (!strEQ(dev, name)) {
            continue;
        }

        found = 1;
        ifstat->rx_bytes    = sigar_strtoul(ptr);
        ifstat->rx_packets  = sigar_strtoul(ptr);
        ifstat->rx_errors   = sigar_strtoul(ptr);
        ifstat->rx_dropped  = sigar_strtoul(ptr);
        ifstat->rx_overruns = sigar_strtoul(ptr);
        ifstat->rx_frame    = sigar_strtoul(ptr);

        /* skip: compressed multicast */
        ptr = sigar_skip_multiple_token(ptr, 2);

        ifstat->tx_bytes      = sigar_strtoul(ptr);
        ifstat->tx_packets    = sigar_strtoul(ptr);
        ifstat->tx_errors     = sigar_strtoul(ptr);
        ifstat->tx_dropped    = sigar_strtoul(ptr);
        ifstat->tx_overruns   = sigar_strtoul(ptr);
        ifstat->tx_collisions = sigar_strtoul(ptr);
        ifstat->tx_carrier    = sigar_strtoul(ptr);
        break;
    }

    fclose(fp);

    return found ? SIGAR_OK : ENXIO;
}

static SIGAR_INLINE int ip_format(char *buffer, int buflen, char *ptr)
{
    int alen = strlen(ptr);

    if (alen > 8) {
        struct in6_addr addr;
        int i;
        for (i=0; i<=3; i++, ptr+=8) {
            addr.s6_addr32[i] = hex2int(ptr);
        }
        if (!inet_ntop(AF_INET6, &addr, buffer, buflen)) {
            return errno;
        }
    }
    else {
        struct in_addr addr;
        addr.s_addr = hex2int(ptr);
        if (!inet_ntop(AF_INET, &addr, buffer, buflen)) {
            return errno;
        }
    }

    return SIGAR_OK;
}

typedef struct {
    sigar_net_connection_list_t *connlist;
    sigar_net_connection_t *conn;
    unsigned long port;
} net_conn_getter_t;

#define CONN_GET_DONE -2

static int proc_net_read(net_conn_getter_t *getter,
                         const char *fname,
                         int flags, int type)
{
    FILE *fp;
    char buffer[8192], *ptr;

    if (!(fp = fopen(fname, "r"))) {
        return errno;
    }

    fgets(buffer, sizeof(buffer), fp); /* skip header */

    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        sigar_net_connection_t conn;
        char *port = NULL;
        char *laddr, *raddr;
        int status;

        ptr = sigar_skip_token(ptr); /* skip number */
        while (isspace(*ptr)) {
            ptr++;
        }

        port = strchr(ptr, ':');
        *port = '\0';
        ++port;

        conn.local_port = (strtoul(port, &port, 16) & 0xffff);
        laddr = ptr;

        ptr = port;
        while (isspace(*ptr)) {
            ptr++;
        }

        port = strchr(ptr, ':');
        *port = '\0';
        ++port;

        conn.remote_port = (strtoul(port, &port, 16) & 0xffff);
        raddr = ptr;

        ptr = port;
        while (isspace(*ptr)) {
            ptr++;
        }

        if (!((conn.remote_port && (flags & SIGAR_NETCONN_CLIENT)) ||
              (!conn.remote_port && (flags & SIGAR_NETCONN_SERVER))))
        {
            continue;
        }

        conn.type = type;

        status = ip_format(conn.local_address,
                           sizeof(conn.local_address),
                           laddr);

        if (status != SIGAR_OK) {
            return status;
        }

        status = ip_format(conn.remote_address,
                           sizeof(conn.remote_address),
                           raddr);

        if (status != SIGAR_OK) {
            return status;
        }

        /* SIGAR_TCP_* currently matches TCP_* in linux/tcp.h */
        sscanf(ptr, "%2x", &conn.state);
        ptr = sigar_skip_token(ptr);
        SIGAR_SKIP_SPACE(ptr);

        conn.send_queue = hex2int(ptr);
        ptr += 9; /* tx + ':' */;
        conn.receive_queue = hex2int(ptr);
        ptr += 8;
        SIGAR_SKIP_SPACE(ptr);

        ptr = sigar_skip_multiple_token(ptr, 2); /* tr:tm->when retrnsmt */

        conn.uid = sigar_strtoul(ptr);

        ptr = sigar_skip_token(ptr);

        conn.inode = sigar_strtoul(ptr);

        if (getter->connlist) {
            SIGAR_NET_CONNLIST_GROW(getter->connlist);
            memcpy(&getter->connlist->data[getter->connlist->number++],
                   &conn, sizeof(conn));
        }
        else {
            if ((getter->port == conn.local_port) &&
                (conn.remote_port == 0))
            {
                memcpy(getter->conn, &conn, sizeof(conn));
                fclose(fp);
                return CONN_GET_DONE;
            }
        }
    }

    fclose(fp);

    return SIGAR_OK;
}

static int net_conn_get(sigar_t *sigar,
                        net_conn_getter_t *getter,
                        int flags)
{
    int status;

    if (flags & SIGAR_NETCONN_TCP) {
        status = proc_net_read(getter,
                               PROC_FS_ROOT "net/tcp",
                               flags, SIGAR_NETCONN_TCP);

        if (status != SIGAR_OK) {
            return status;
        }

        status = proc_net_read(getter,
                               PROC_FS_ROOT "net/tcp6",
                               flags, SIGAR_NETCONN_TCP);

        if (!((status == SIGAR_OK) || (status == ENOENT))) {
            return status;
        }
    }

    if (flags & SIGAR_NETCONN_UDP) {
        status = proc_net_read(getter,
                               PROC_FS_ROOT "net/udp",
                               flags, SIGAR_NETCONN_UDP);

        if (status != SIGAR_OK) {
            return status;
        }

        status = proc_net_read(getter,
                               PROC_FS_ROOT "net/udp6",
                               flags, SIGAR_NETCONN_UDP);

        if (!((status == SIGAR_OK) || (status == ENOENT))) {
            return status;
        }
    }

    if (flags & SIGAR_NETCONN_RAW) {
        status = proc_net_read(getter,
                               PROC_FS_ROOT "net/raw",
                               flags, SIGAR_NETCONN_RAW);

        if (status != SIGAR_OK) {
            return status;
        }

        status = proc_net_read(getter,
                               PROC_FS_ROOT "net/raw6",
                               flags, SIGAR_NETCONN_RAW);

        if (!((status == SIGAR_OK) || (status == ENOENT))) {
            return status;
        }
    }

    /* XXX /proc/net/unix */

    return SIGAR_OK;
}

int sigar_net_connection_list_get(sigar_t *sigar,
                                  sigar_net_connection_list_t *connlist,
                                  int flags)
{
    int status;
    net_conn_getter_t getter;

    sigar_net_connection_list_create(connlist);

    getter.conn = NULL;
    getter.connlist = connlist;

    status = net_conn_get(sigar, &getter, flags);

    if (status != SIGAR_OK) {
        sigar_net_connection_list_destroy(sigar, connlist);
    }

    return status;
}

static int sigar_net_connection_get(sigar_t *sigar,
                                    sigar_net_connection_t *netconn,
                                    unsigned long port,
                                    int flags)
{
    int status;
    net_conn_getter_t getter;

    getter.conn = netconn;
    getter.connlist = NULL;
    getter.port = port;

    status = net_conn_get(sigar, &getter, flags);

    if (status == CONN_GET_DONE) {
        return SIGAR_OK;
    }

    return status;
}

int sigar_proc_port_get(sigar_t *sigar, int protocol,
                        unsigned long port, sigar_pid_t *pid)
{
    int status;
    sigar_net_connection_t netconn;
    DIR *dirp;
    struct dirent *ent, dbuf;

    SIGAR_ZERO(&netconn);
    *pid = 0;

    status = sigar_net_connection_get(sigar, &netconn, port,
                                      SIGAR_NETCONN_SERVER|protocol);

    if (status != SIGAR_OK) {
        return status;
    }

    if (netconn.local_port != port) {
        return SIGAR_OK; /* XXX or ENOENT? */
    }

    if (!(dirp = opendir(PROCP_FS_ROOT))) {
        return errno;
    }

    while (readdir_r(dirp, &dbuf, &ent) == 0) {
        DIR *fd_dirp;
        struct dirent *fd_ent, fd_dbuf;
        struct stat sb;
        char fd_name[BUFSIZ], pid_name[BUFSIZ];
        int len, slen;

        if (ent == NULL) {
            break;
        }

        if (!sigar_isdigit(*ent->d_name)) {
            continue;
        }

        /* sprintf(pid_name, "/proc/%s", ent->d_name) */
        memcpy(&pid_name[0], PROCP_FS_ROOT, SSTRLEN(PROCP_FS_ROOT));
        len = SSTRLEN(PROCP_FS_ROOT);
        pid_name[len++] = '/';

        slen = strlen(ent->d_name);
        memcpy(&pid_name[len], ent->d_name, slen);
        len += slen;
        pid_name[len] = '\0';
        
        if (stat(pid_name, &sb) < 0) {
            continue;
        }
        if (sb.st_uid != netconn.uid) {
            continue;
        }

        /* sprintf(fd_name, "%s/fd", pid_name) */
        memcpy(&fd_name[0], pid_name, len);
        memcpy(&fd_name[len], "/fd", 3);
        fd_name[len+=3] = '\0';

        if (!(fd_dirp = opendir(fd_name))) {
            continue;
        }

        while (readdir_r(fd_dirp, &fd_dbuf, &fd_ent) == 0) {
            char fd_ent_name[BUFSIZ];

            if (fd_ent == NULL) {
                break;
            }

            if (!sigar_isdigit(*fd_ent->d_name)) {
                continue;
            }

            /* sprintf(fd_ent_name, "%s/%s", fd_name, fd_ent->d_name) */
            slen = strlen(fd_ent->d_name);
            memcpy(&fd_ent_name[0], fd_name, len);
            fd_ent_name[len] = '/';
            memcpy(&fd_ent_name[len+1], fd_ent->d_name, slen);
            fd_ent_name[len+1+slen] = '\0';

            if (stat(fd_ent_name, &sb) < 0) {
                continue;
            }

            if (sb.st_ino == netconn.inode) {
                closedir(fd_dirp);
                closedir(dirp);
                *pid = strtoul(ent->d_name, NULL, 10);
                return SIGAR_OK;
            }
            
        }

        closedir(fd_dirp);
    }

    closedir(dirp);

    return SIGAR_OK;
}
