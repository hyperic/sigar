#include "sigar.h"
#include "sigar_private.h"
#include "sigar_pdh.h"
#include "sigar_os.h"
#include "sigar_util.h"
#include <shellapi.h>

#define USING_WIDE_S(s) (s)->using_wide
#define USING_WIDE()    USING_WIDE_S(sigar)

#define PERFBUF_SIZE 8192

#define PERF_TITLE_PROC       230
#define PERF_TITLE_PROC_KEY  "230"
#define PERF_TITLE_CPU_KEY   "238"

#define PERF_TITLE_CPU_USER    142
#define PERF_TITLE_CPU_IDLE    1746
#define PERF_TITLE_CPU_SYS     144

typedef enum {
    PERF_IX_CPU_USER,
    PERF_IX_CPU_IDLE,
    PERF_IX_CPU_SYS,
    PERF_IX_CPU_MAX
} perf_cpu_offsets_t;

#define PERF_TITLE_CPUTIME    6
#define PERF_TITLE_MEM_VSIZE  174
#define PERF_TITLE_MEM_SIZE   180
#define PERF_TITLE_MEM_PRIV   186
#define PERF_TITLE_THREAD_CNT 680
#define PERF_TITLE_HANDLE_CNT 952
#define PERF_TITLE_PID        784
#define PERF_TITLE_PPID       1410
#define PERF_TITLE_PRIORITY   682
#define PERF_TITLE_START_TIME 684

typedef enum {
    PERF_IX_CPUTIME,
    PERF_IX_MEM_VSIZE,
    PERF_IX_MEM_SIZE,
    PERF_IX_MEM_PRIV,
    PERF_IX_THREAD_CNT,
    PERF_IX_HANDLE_CNT,
    PERF_IX_PID,
    PERF_IX_PPID,
    PERF_IX_PRIORITY,
    PERF_IX_START_TIME,
    PERF_IX_MAX
} perf_proc_offsets_t;

/* 
 * diff is:
 *   ExW      -> ExA
 *   wcounter -> counter
 */
#define MyRegQueryValue() \
    (USING_WIDE() ? \
        RegQueryValueExW(sigar->handle, \
                         wcounter_key, NULL, &type, \
                         sigar->perfbuf, \
                         &sigar->perfbuf_size) : \
        RegQueryValueExA(sigar->handle, \
                         counter_key, NULL, &type, \
                         sigar->perfbuf, \
                         &sigar->perfbuf_size))

#define PERF_VAL(ix) \
    perf_offsets[ix] ? \
        *((DWORD *)((BYTE *)counter_block + perf_offsets[ix])) : 0

static PERF_OBJECT_TYPE *get_perf_object(sigar_t *sigar, char *counter_key,
                                         DWORD *err)
{
    DWORD retval, type;
    WCHAR wcounter_key[MAX_PATH+1];
    PERF_DATA_BLOCK *block;

    *err = SIGAR_OK;

    if (!sigar->perfbuf) {
        sigar->perfbuf = malloc(PERFBUF_SIZE);
        sigar->perfbuf_size = PERFBUF_SIZE;
    }

    if (USING_WIDE()) {
        SIGAR_A2W(counter_key, wcounter_key, sizeof(wcounter_key));
    }

    while ((retval = MyRegQueryValue()) != ERROR_SUCCESS) {
        if (retval == ERROR_MORE_DATA) {
            sigar->perfbuf_size += PERFBUF_SIZE;

            sigar->perfbuf =
                realloc(sigar->perfbuf, sigar->perfbuf_size);
        }
        else {
            *err = retval;
            return NULL;
        }
    }

    block = (PERF_DATA_BLOCK *)sigar->perfbuf;

    return PdhFirstObject(block);
}

static void get_sysinfo(sigar_t *sigar)
{
    SYSTEM_INFO sysinfo;

    GetSystemInfo(&sysinfo);

    sigar->ncpu = sysinfo.dwNumberOfProcessors;
    sigar->pagesize = sysinfo.dwPageSize;
}

/* for C# bindings */
SIGAR_DECLARE(sigar_t *) sigar_new(void)
{
    sigar_t *sigar;
    if (sigar_open(&sigar) != SIGAR_OK) {
        return NULL;
    }
    return sigar;
}

int sigar_os_open(sigar_t **sigar)
{
    LONG result;
    HINSTANCE h;
    OSVERSIONINFO version;

    *sigar = malloc(sizeof(**sigar));
    (*sigar)->machine = ""; /* local machine */
    (*sigar)->using_wide = 0; /*XXX*/

    (*sigar)->perfbuf = NULL;
    (*sigar)->perfbuf_size = 0;

    version.dwOSVersionInfoSize = sizeof(version);
    GetVersionEx(&version);

    /*
     * 4 == NT 4.0
     * 5 == 2000, XP, 2003 Server
     */
    (*sigar)->winnt = (version.dwMajorVersion == 4);

    if (USING_WIDE_S(*sigar)) {
        WCHAR wmachine[MAX_PATH+1];

        SIGAR_A2W((*sigar)->machine, wmachine, sizeof(wmachine));

        result = RegConnectRegistryW(wmachine,
                                     HKEY_PERFORMANCE_DATA,
                                     &(*sigar)->handle);
    }
    else {
        result = RegConnectRegistryA((*sigar)->machine,
                                     HKEY_PERFORMANCE_DATA,
                                     &(*sigar)->handle);
    }

    get_sysinfo(*sigar);

    if ((h = LoadLibrary("iphlpapi.dll"))) {
        (*sigar)->get_if_table = 
            (LPGETIFTABLE)GetProcAddress(h, "GetIfTable");
        (*sigar)->get_ipforward_table = 
            (LPGETIPFORWARDTABLE)GetProcAddress(h, "GetIpForwardTable");
        (*sigar)->get_tcp_table = 
            (LPGETTCPTABLE)GetProcAddress(h, "GetTcpTable");
        (*sigar)->get_tcpx_table = 
            (LPGETTCPEXTABLE)GetProcAddress(h,
                                            "AllocateAndGet"
                                            "TcpExTableFromStack");
        (*sigar)->get_udp_table = 
            (LPGETUDPTABLE)GetProcAddress(h, "GetUdpTable");
    }
    else {
        (*sigar)->get_if_table = NULL;
        (*sigar)->get_ipforward_table = NULL;
    }

    (*sigar)->ip_handle = h;
    (*sigar)->pinfo.pid = -1;
    (*sigar)->ws_version = 0;
    (*sigar)->ncpu = 0;
    (*sigar)->peb = NULL;

    return result;
}

int sigar_os_close(sigar_t *sigar)
{
    int retval;

    if (sigar->perfbuf) {
        free(sigar->perfbuf);
    }

    retval = RegCloseKey(sigar->handle);

    if (sigar->ip_handle) {
        FreeLibrary(sigar->ip_handle);
    }

    if (sigar->ws_version != 0) {
        WSACleanup();
    }

    if (sigar->peb) {
        free(sigar->peb);
    }

    free(sigar);

    return retval;
}

char *sigar_os_error_string(int err)
{
    return NULL;
}

int sigar_wsa_init(sigar_t *sigar)
{
    if (sigar->ws_version == 0) {
        WSADATA data;

        if (WSAStartup(MAKEWORD(2, 0), &data)) {
            sigar->ws_error = WSAGetLastError();
            WSACleanup();
            return sigar->ws_error;
        }

        sigar->ws_version = data.wVersion;
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_mem_get(sigar_t *sigar, sigar_mem_t *mem)
{
    MEMORYSTATUS memstat;

    GlobalMemoryStatus(&memstat);

    mem->total  = memstat.dwTotalPhys;
    mem->free   = memstat.dwAvailPhys;
    mem->shared = memstat.dwTotalVirtual - memstat.dwAvailVirtual;
    mem->used   = mem->total - mem->free;

    sigar_mem_calc_ram(sigar, mem);

    /*XXX*/
    mem->buffer = 0;
    mem->cached = 0;

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_swap_get(sigar_t *sigar, sigar_swap_t *swap)
{
    MEMORYSTATUS memstat;

    GlobalMemoryStatus(&memstat);

    swap->total = memstat.dwTotalPageFile;
    swap->free  = memstat.dwAvailPageFile;
    swap->used  = swap->total - swap->free;

    return SIGAR_OK;
}

static PERF_INSTANCE_DEFINITION *get_cpu_instance(sigar_t *sigar,
                                                  DWORD *perf_offsets,
                                                  DWORD *num, DWORD *err)
{
    PERF_OBJECT_TYPE *object = get_perf_object(sigar, "238", err);
    PERF_INSTANCE_DEFINITION *inst;
    PERF_COUNTER_DEFINITION *counter;
    DWORD i;

    if (!object) {
        return NULL;
    }

    for (i=0, counter = PdhFirstCounter(object);
         i<object->NumCounters;
         i++, counter = PdhNextCounter(counter))
    {
        DWORD offset = counter->CounterOffset;

        switch (counter->CounterNameTitleIndex) {
          case PERF_TITLE_CPU_SYS:
            perf_offsets[PERF_IX_CPU_SYS] = offset;
            break;
          case PERF_TITLE_CPU_USER:
            perf_offsets[PERF_IX_CPU_USER] = offset;
            break;
          case PERF_TITLE_CPU_IDLE:
            perf_offsets[PERF_IX_CPU_IDLE] = offset;
            break;
        }
    }

    if (num) {
        *num = object->NumInstances;
    }

    return PdhFirstInstance(object);
}

SIGAR_DECLARE(int) sigar_cpu_get(sigar_t *sigar, sigar_cpu_t *cpu)
{
    PERF_INSTANCE_DEFINITION *inst;
    PERF_COUNTER_BLOCK *counter_block;
    DWORD perf_offsets[PERF_IX_CPU_MAX], err;

    SIGAR_ZERO(cpu);
    memset(&perf_offsets, 0, sizeof(perf_offsets));

    inst = get_cpu_instance(sigar, (DWORD*)&perf_offsets, 0, &err);

    if (!inst) {
        return GetLastError();
    }

    /* first instance is total, rest are per-cpu */
    counter_block = PdhGetCounterBlock(inst);

    cpu->sys  = PERF_VAL(PERF_IX_CPU_SYS);
    cpu->user = PERF_VAL(PERF_IX_CPU_USER);
    cpu->idle = PERF_VAL(PERF_IX_CPU_IDLE);
    cpu->nice = 0; /* no nice here */

    cpu->total = cpu->sys + cpu->user + cpu->idle;
    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_cpu_list_get(sigar_t *sigar, sigar_cpu_list_t *cpulist)
{
    int status, i, j, hthread=0;
    PERF_INSTANCE_DEFINITION *inst;
    DWORD perf_offsets[PERF_IX_CPU_MAX], num, err;

    memset(&perf_offsets, 0, sizeof(perf_offsets));

    /* first instance is total, rest are per-cpu */
    inst = get_cpu_instance(sigar, (DWORD*)&perf_offsets, &num, &err);

    if (!inst) {
        return err;
    }

    if (!sigar->winnt) {
        /* skip Processor _Total instance (NT doesnt have one) */
        --num;
        inst = PdhNextInstance(inst);
    }

    sigar_cpu_count(sigar);
    sigar_cpu_list_create(cpulist);

    /*
     * if hyper-threading was detected and ncpu is less than
     * the number of counter instances, assume there is a counter
     * for each logical processor.
     * XXX assuming this is correct until have something to test on.
     */
    if (sigar->ht_enabled && ((sigar->ncpu * sigar->lcpu) == num)) {
        hthread = 1;
    }

    for (i=0; i<num; i++) {
        PERF_COUNTER_BLOCK *counter_block;
        sigar_cpu_t *cpu;

        if (hthread && (i % sigar->lcpu)) {
            /* merge times of logical processors */
            cpu = &cpulist->data[cpulist->number-1];
        }
        else {
            SIGAR_CPU_LIST_GROW(cpulist);
            cpu = &cpulist->data[cpulist->number++];
            SIGAR_ZERO(cpu);
        }

        counter_block = PdhGetCounterBlock(inst);

        cpu->sys  += PERF_VAL(PERF_IX_CPU_SYS);
        cpu->user += PERF_VAL(PERF_IX_CPU_USER);
        cpu->idle += PERF_VAL(PERF_IX_CPU_IDLE);
        cpu->nice = 0; /* no nice here */
        
        cpu->total += cpu->sys + cpu->user + cpu->idle;

        inst = PdhNextInstance(inst);
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_uptime_get(sigar_t *sigar,
                                    sigar_uptime_t *uptime)
{
    uptime->uptime = GetTickCount() / 1000;
    return SIGAR_OK;
}

/*
 * there is no api for this info.
 * closest i've seen is enumerating the entire process table
 * and calculating an average based on process times.
 */
SIGAR_DECLARE(int) sigar_loadavg_get(sigar_t *sigar,
                                     sigar_loadavg_t *loadavg)
{
    return SIGAR_ENOTIMPL;
}

#define get_process_object(sigar, err) \
    get_perf_object(sigar, PERF_TITLE_PROC_KEY, err)

SIGAR_DECLARE(int) sigar_proc_list_get(sigar_t *sigar,
                                       sigar_proc_list_t *proclist)
{
    PERF_OBJECT_TYPE *object;
    PERF_INSTANCE_DEFINITION *inst;
    PERF_COUNTER_DEFINITION *counter;
    DWORD i, err;
    DWORD perf_offsets[PERF_IX_MAX];

    perf_offsets[PERF_IX_PID] = 0;

    object = get_process_object(sigar, &err);

    if (!object) {
        return err;
    }

    sigar_proc_list_create(proclist);

    /*
     * note we assume here:
     *  block->NumObjectTypes == 1
     *  object->ObjectNameTitleIndex == PERF_TITLE_PROC
     *
     * which should always be the case.
     */

    for (i=0, counter = PdhFirstCounter(object);
         i<object->NumCounters;
         i++, counter = PdhNextCounter(counter))
    {
        DWORD offset = counter->CounterOffset;

        switch (counter->CounterNameTitleIndex) {
          case PERF_TITLE_PID:
            perf_offsets[PERF_IX_PID] = offset;
            break;
        }
    }

    for (i=0, inst = PdhFirstInstance(object);
         i<object->NumInstances;
         i++, inst = PdhNextInstance(inst))
    {
        PERF_COUNTER_BLOCK *counter_block = PdhGetCounterBlock(inst);
        DWORD pid = PERF_VAL(PERF_IX_PID);

        if (pid == 0) {
            continue; /* dont include the system Idle process */
        }

        SIGAR_PROC_LIST_GROW(proclist);

        proclist->data[proclist->number++] = pid;
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_proc_stat_get(sigar_t *sigar,
                                       sigar_proc_stat_t *procstat)
{
    int status = /* XXX optimize */
        sigar_proc_count(sigar, &procstat->total);

    return status;
}

#define PROCESS_DAC (PROCESS_QUERY_INFORMATION|PROCESS_VM_READ)

static HANDLE open_process(sigar_pid_t pid)
{
    return OpenProcess(PROCESS_DAC, 0, (DWORD)pid);
}

SIGAR_DECLARE(int) sigar_proc_mem_get(sigar_t *sigar, sigar_pid_t pid,
                                      sigar_proc_mem_t *procmem)
{
    int status = get_proc_info(sigar, pid);
    sigar_win32_pinfo_t *pinfo = &sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    procmem->vsize    = pinfo->vsize;
    procmem->size     = pinfo->size;
    procmem->resident = -1;
    procmem->share    = -1;
    procmem->rss      = -1;

    return SIGAR_OK;
}

#define TOKEN_DAC (STANDARD_RIGHTS_READ | READ_CONTROL | TOKEN_QUERY)

SIGAR_DECLARE(int)
sigar_proc_cred_name_get(sigar_t *sigar, sigar_pid_t pid,
                         sigar_proc_cred_name_t *proccredname)
{
    HANDLE proc, token;
    DWORD len;
    int success;
    TOKEN_USER *user = NULL;
    TOKEN_PRIMARY_GROUP *group = NULL;
    SID_NAME_USE type;
    char domain[SIGAR_CRED_NAME_MAX];

    /* XXX cache lookup */

    if (!(proc = open_process(pid))) {
        return GetLastError();
    }
    
    if (!OpenProcessToken(proc, TOKEN_DAC, &token)) {
        CloseHandle(proc);
        return GetLastError();
    }

    CloseHandle(proc);

    success =
        !GetTokenInformation(token, TokenUser, NULL, 0, &len) &&
        (GetLastError() == ERROR_INSUFFICIENT_BUFFER) &&
        (user = malloc(len)) &&
        GetTokenInformation(token, TokenUser, user, len, &len);

    if (success) {
        DWORD domain_len = sizeof(domain);
        DWORD user_len = sizeof(proccredname->user);

        success = LookupAccountSid(NULL, user->User.Sid,
                                   proccredname->user, &user_len,
                                   domain, &domain_len, &type);
    }

    if (user != NULL) {
        free(user);
    }
    if (!success) {
        CloseHandle(token);
        return GetLastError();
    }

    success =
        !GetTokenInformation(token, TokenPrimaryGroup, NULL, 0, &len) &&
        (GetLastError() == ERROR_INSUFFICIENT_BUFFER) &&
        (group = malloc(len)) &&
        GetTokenInformation(token, TokenPrimaryGroup, group, len, &len);

    if (success) {
        DWORD domain_len = sizeof(domain);
        DWORD group_len = sizeof(proccredname->group);

        success = LookupAccountSid(NULL, group->PrimaryGroup,
                                   proccredname->group, &group_len,
                                   domain, &domain_len, &type);
    }

    if (group != NULL) {
        free(group);
    }

    CloseHandle(token);

    if (!success) {
        return GetLastError();
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_proc_cred_get(sigar_t *sigar, sigar_pid_t pid,
                                       sigar_proc_cred_t *proccred)
{
    return SIGAR_ENOTIMPL;
}

#define FILETIME2SEC(ft) \
    (((ft.dwHighDateTime << 32) | ft.dwLowDateTime) / 10000000)

SIGAR_DECLARE(int) sigar_proc_time_get(sigar_t *sigar, sigar_pid_t pid,
                                       sigar_proc_time_t *proctime)
{
    HANDLE proc = open_process(pid);
    FILETIME start_time, exit_time, system_time, user_time;

    if (!proc) {
        return GetLastError();
    }

    if (!GetProcessTimes(proc,
                         &start_time, &exit_time,
                         &system_time, &user_time))
    {
        return GetLastError();
    }

    CloseHandle(proc);

    if (start_time.dwHighDateTime) {
        proctime->start_time = FileTimeToTime(&start_time) / 1000;
    }
    else {
        proctime->start_time = 0;
    }

    proctime->utime = FILETIME2SEC(user_time);
    proctime->stime = FILETIME2SEC(system_time);

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_proc_state_get(sigar_t *sigar, sigar_pid_t pid,
					sigar_proc_state_t *procstate)
{
    int status = get_proc_info(sigar, pid);
    sigar_win32_pinfo_t *pinfo = &sigar->pinfo;

    if (status != SIGAR_OK) {
        return status;
    }

    memcpy(procstate->name, pinfo->name, sizeof(procstate->name));
    procstate->state = pinfo->state;
    procstate->ppid = pinfo->ppid;
    procstate->priority = pinfo->priority;
    procstate->nice = -1;
    procstate->tty = -1;

    return SIGAR_OK;
}

static int get_proc_info(sigar_t *sigar, sigar_pid_t pid)
{
    PERF_OBJECT_TYPE *object;
    PERF_INSTANCE_DEFINITION *inst;
    PERF_COUNTER_DEFINITION *counter;
    DWORD i, err;
    DWORD perf_offsets[PERF_IX_MAX];
    sigar_win32_pinfo_t *pinfo = &sigar->pinfo;
    time_t timenow = time(NULL);

    if (pinfo->pid == pid) {
        if ((timenow - pinfo->mtime) < SIGAR_LAST_PROC_EXPIRE) {
            return SIGAR_OK;
        }
    }

    pinfo->pid = pid;
    pinfo->mtime = timenow;

    memset(&perf_offsets, 0, sizeof(perf_offsets));

    object = get_process_object(sigar, &err);

    if (object == NULL) {
        return err;
    }

    /*
     * note we assume here:
     *  block->NumObjectTypes == 1
     *  object->ObjectNameTitleIndex == PERF_TITLE_PROC
     *
     * which should always be the case.
     */

    for (i=0, counter = PdhFirstCounter(object);
         i<object->NumCounters;
         i++, counter = PdhNextCounter(counter))
    {
        DWORD offset = counter->CounterOffset;

        switch (counter->CounterNameTitleIndex) {
          case PERF_TITLE_CPUTIME:
            perf_offsets[PERF_IX_CPUTIME] = offset;
            break;
          case PERF_TITLE_MEM_VSIZE:
            perf_offsets[PERF_IX_MEM_VSIZE] = offset;
            break;
          case PERF_TITLE_MEM_SIZE:
            perf_offsets[PERF_IX_MEM_SIZE] = offset;
            break;
          case PERF_TITLE_MEM_PRIV:
            perf_offsets[PERF_IX_MEM_PRIV] = offset;
            break;
          case PERF_TITLE_THREAD_CNT:
            perf_offsets[PERF_IX_THREAD_CNT] = offset;
            break;
          case PERF_TITLE_HANDLE_CNT:
            perf_offsets[PERF_IX_HANDLE_CNT] = offset;
            break;
          case PERF_TITLE_PID:
            perf_offsets[PERF_IX_PID] = offset;
            break;
          case PERF_TITLE_PPID:
            perf_offsets[PERF_IX_PPID] = offset;
            break;
          case PERF_TITLE_PRIORITY:
            perf_offsets[PERF_IX_PRIORITY] = offset;
            break;
          case PERF_TITLE_START_TIME:
            perf_offsets[PERF_IX_START_TIME] = offset;
            break;
        }
    }

    for (i=0, inst = PdhFirstInstance(object);
         i<object->NumInstances;
         i++, inst = PdhNextInstance(inst))
    {
        PERF_COUNTER_BLOCK *counter_block = PdhGetCounterBlock(inst);
        sigar_pid_t this_pid = PERF_VAL(PERF_IX_PID);

        if (this_pid != pid) {
            continue;
        }

        pinfo->state = 'R'; /* XXX? */
        SIGAR_W2A(PdhInstanceName(inst),
                  pinfo->name, sizeof(pinfo->name));

        pinfo->size     = PERF_VAL(PERF_IX_MEM_SIZE);
        pinfo->vsize    = PERF_VAL(PERF_IX_MEM_VSIZE);
        pinfo->ppid     = PERF_VAL(PERF_IX_PPID);
        pinfo->priority = PERF_VAL(PERF_IX_PRIORITY);
        pinfo->handles  = PERF_VAL(PERF_IX_HANDLE_CNT);

        return SIGAR_OK;
    }

    return -1; /*XXX*/
}

static char *getarg(char **line)
{
    char *str = *line, *end;
    char *res;
    int len;

    while (*str && isspace(*str)) {
        ++str;
    }

    if (!*str) {
        *line = str;
        return NULL;
    }

    if (*str == '"') {
        end = str + 1;

        while (*end && (*end != '"')) {
            ++end;
        }

        len = end - str - 1;
        res = malloc(len+1);
        memcpy(res, str+1, len);
        res[len] = '\0';
        end++;
    }
    else {
        end = str;
        while (*end && !isspace(*end)) {
            ++end;
        }

        len = end - str;
        res = malloc(len+1);
        memcpy(res, str, len);
        res[len] = '\0';
    }

    while (*end && isspace(*end)) {
        ++end;
    }

    *line = end;

    return res;
}

#if 0
/* this was the first shot, works but injects a remote thread
 * in the process.  probably best to avoid that.
 */
/*
 * this is ugly, but there is no alternative.
 * we spawn a remote thread within the process
 * to call GetCommandLine() and parse it ourselves.
 */
static int sigar_remote_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                                      sigar_proc_args_t *procargs)
{
    FARPROC rcmdline, fstrlen;
    HANDLE proc, thread, kdll;
    PVOID data=NULL;
    char *cmdline;
    DWORD rv, thrid, bytes, datalen=0;

    if (!(kdll = GetModuleHandle("kernel32.dll"))) {
        return GetLastError();
    }

    if (!(rcmdline = GetProcAddress(kdll, "GetCommandLineA"))) {
        return GetLastError();
    }

    if (!(fstrlen = GetProcAddress(kdll, "lstrlenA"))) {
        return GetLastError();
    }

    if (!(proc = OpenProcess(MAXIMUM_ALLOWED, 0, (DWORD)pid))) {
        return GetLastError();
    }

    thread = CreateRemoteThread(proc, NULL, 0,
                                (LPTHREAD_START_ROUTINE)rcmdline,
                                0, 0, &thrid);
    if (!thread) {
        CloseHandle(proc);
        return GetLastError(); 
    }

    WaitForSingleObject(thread, INFINITE);
    GetExitCodeThread(thread, (LPDWORD)(&data));
    CloseHandle(thread);

    if (!data) {
        CloseHandle(proc);
        return GetLastError(); 
    }

    thread = CreateRemoteThread(proc, NULL, 0,
                                (LPTHREAD_START_ROUTINE)fstrlen,
                                data, 0, &thrid);
    if (!thread) {
        CloseHandle(proc);
        return GetLastError();
    }

    WaitForSingleObject(thread, INFINITE);
    GetExitCodeThread(thread, &datalen);
    CloseHandle(thread);

    if (!datalen) {
        CloseHandle(proc);
        return GetLastError();
    }

    datalen++; /* we want the \0 terminator too */

    cmdline = HeapAlloc(GetProcessHeap(),
                        HEAP_ZERO_MEMORY,
                        datalen);

    if (!cmdline) {
        CloseHandle(proc);
        return GetLastError();
    }

    if (ReadProcessMemory(proc, data, cmdline,
                          datalen, &bytes))
    {
        char *arg, *ptr = cmdline;

        sigar_proc_args_create(procargs);

        while (*ptr && (arg = getarg(&ptr))) {
            SIGAR_PROC_ARGS_GROW(procargs);
            procargs->data[procargs->number++] = arg;
        }

        HeapFree(GetProcessHeap(), 0, cmdline);
    }
    else {
        CloseHandle(proc);
        return GetLastError();
    }

    CloseHandle(proc);
    return SIGAR_OK;
}
#else
/* second shot, using the PEB.
 * read-only, minimum access privs and in general seems to work
 * better than the method above.  lets roll...
 */
static int sigar_remote_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                                      sigar_proc_args_t *procargs)
{
    int status;
    char cmdline[MAX_PATH], *ptr = cmdline, *arg;
    HANDLE proc = open_process(pid);

    if (!proc) {
        return GetLastError();
    }
    
    status = sigar_proc_cmdline_get(sigar, proc, cmdline);

    CloseHandle(proc);

    if (status != SIGAR_OK) {
        return status;
    }

    sigar_proc_args_create(procargs);

    while (*ptr && (arg = getarg(&ptr))) {
        SIGAR_PROC_ARGS_GROW(procargs);
        procargs->data[procargs->number++] = strdup(arg);
    }

    return SIGAR_OK;
}
#endif

static int sigar_local_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                                     sigar_proc_args_t *procargs)
{
    LPTSTR cmdline = GetCommandLine();
    char *arg, *ptr = cmdline;

    sigar_proc_args_create(procargs);

    while (*ptr && (arg = getarg(&ptr))) {
        SIGAR_PROC_ARGS_GROW(procargs);
        procargs->data[procargs->number++] = arg;
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_proc_args_get(sigar_t *sigar, sigar_pid_t pid,
                                       sigar_proc_args_t *procargs)
{
    if (pid == sigar->pid) {
        return sigar_local_proc_args_get(sigar, pid, procargs);
    }
    else {
        return sigar_remote_proc_args_get(sigar, pid, procargs);
    }
}

static int sigar_local_proc_env_get(sigar_t *sigar, sigar_pid_t pid,
                                    sigar_proc_env_t *procenv)
{
    UCHAR *ptr, *env;

    env = ptr = (UCHAR*)GetEnvironmentStrings();

    while (*ptr) {
        char *val;
        int klen, vlen, status;
        char key[128]; /* XXX is there a max key size? */

        if (*ptr == '=') {
            ptr += strlen(ptr)+1;
            continue;
        }

        val = strchr(ptr, '=');

        if (val == NULL) {
            break; /*XXX*/
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

        ptr += klen + 1 + vlen + 1;
    }

    FreeEnvironmentStrings(env);

    return SIGAR_OK;
}

static int sigar_remote_proc_env_get(sigar_t *sigar, sigar_pid_t pid,
                                     sigar_proc_env_t *procenv)
{
    FARPROC rgetenv, fstrlen;
    HANDLE proc, thread, kdll;
    PVOID data=NULL;
    const char *key;
    char *value;
    DWORD rv, thrid, bytes, datalen=0, size;
    LPVOID addr;

    if (!(kdll = GetModuleHandle("msvcrt.dll"))) {
        return GetLastError();
    }

    if (!(rgetenv = GetProcAddress(kdll, "getenv"))) {
        return GetLastError();
    }

    if (!(kdll = GetModuleHandle("kernel32.dll"))) {
        return GetLastError();
    }

    if (!(fstrlen = GetProcAddress(kdll, "lstrlenA"))) {
        return GetLastError();
    }

    if (!(proc = OpenProcess(MAXIMUM_ALLOWED, 0, (DWORD)pid))) {
        return GetLastError();
    }

    key  = procenv->key;
    size = procenv->klen+1;
    addr = VirtualAllocEx(proc, NULL, size,
                          MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    if (!addr) {
        CloseHandle(proc);
        return GetLastError(); 
    }

    if (!WriteProcessMemory(proc, addr, (char*)&key[0], size, 0)) {
        VirtualFreeEx(proc, addr, size, 0);
        CloseHandle(proc);
        return GetLastError(); 
    }

    thread = CreateRemoteThread(proc, NULL, 0,
                                (LPTHREAD_START_ROUTINE)rgetenv,
                                addr, 0, &thrid);
    if (!thread) {
        VirtualFreeEx(proc, addr, size, 0);
        CloseHandle(proc);
        return GetLastError(); 
    }

    WaitForSingleObject(thread, INFINITE);
    GetExitCodeThread(thread, (LPDWORD)(&data));
    CloseHandle(thread);
    VirtualFreeEx(proc, addr, size, 0);

    if (!data) {
        CloseHandle(proc);
        return SIGAR_OK;
    }

    thread = CreateRemoteThread(proc, NULL, 0,
                                (LPTHREAD_START_ROUTINE)fstrlen,
                                data, 0, &thrid);
    if (!thread) {
        CloseHandle(proc);
        return GetLastError();
    }

    WaitForSingleObject(thread, INFINITE);
    GetExitCodeThread(thread, &datalen);
    CloseHandle(thread);

    if (!datalen) {
        CloseHandle(proc);
        return GetLastError();
    }

    value = HeapAlloc(GetProcessHeap(),
                      HEAP_ZERO_MEMORY,
                      datalen);

    if (!value) {
        CloseHandle(proc);
        return GetLastError();
    }

    if (ReadProcessMemory(proc, data, value,
                          datalen+1, &bytes))
    {
        procenv->env_getter(procenv->data,
                            key, strlen(key),
                            value, bytes-1);

        HeapFree(GetProcessHeap(), 0, value);
    }
    else {
        CloseHandle(proc);
        return GetLastError();
    }

    CloseHandle(proc);
    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_proc_env_get(sigar_t *sigar, sigar_pid_t pid,
                                      sigar_proc_env_t *procenv)
{
    if (pid == sigar->pid) {
        if (procenv->type == SIGAR_PROC_ENV_KEY) {
            char value[32767]; /* max size from msdn docs */
            DWORD retval = 
                GetEnvironmentVariable(procenv->key, value, sizeof(value));

            if (retval == 0) {
                if (GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
                    return SIGAR_OK;
                }
                return GetLastError();
            }
            else if (retval > sizeof(value)) {
                /* XXX shouldnt happen */
                return GetLastError();
            }

            procenv->env_getter(procenv->data,
                                procenv->key, procenv->klen,
                                value, retval);
            return SIGAR_OK;
        }
        else {
            return sigar_local_proc_env_get(sigar, pid, procenv);
        }
    }
    else {
        if (procenv->type == SIGAR_PROC_ENV_KEY) {
            return sigar_remote_proc_env_get(sigar, pid, procenv);
        }
        else {
            return SIGAR_ENOTIMPL;
        }
    }
}

SIGAR_DECLARE(int) sigar_proc_fd_get(sigar_t *sigar, sigar_pid_t pid,
                                     sigar_proc_fd_t *procfd)
{
    int status;
    sigar_win32_pinfo_t *pinfo = &sigar->pinfo;

    pinfo->pid = -1; /* force update */
    if ((status = get_proc_info(sigar, pid)) != SIGAR_OK) {
        return status;
    }

    procfd->total = pinfo->handles;

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_proc_exe_get(sigar_t *sigar, sigar_pid_t pid,
                                      sigar_proc_exe_t *procexe)
{
    int status = SIGAR_OK;
    HANDLE proc = open_process(pid);

    if (!proc) {
        return GetLastError();
    }

    procexe->cwd[0] = '\0';
    procexe->root[0] = '\0';

    status = sigar_proc_exe_name_get(sigar, proc, procexe->name);

    return status;
}

typedef HANDLE (CALLBACK *LPCREATESNAPSHOT)(DWORD, DWORD);
typedef BOOL (CALLBACK *LPMODULEITER)(HANDLE, LPMODULEENTRY32);

/* not available on NT */
static int sigar_proc_modules_get_toolhelp(sigar_t *sigar,
                                           sigar_pid_t pid,
                                           sigar_proc_modules_t *procmods)
{
    HINSTANCE k32_handle;
    HANDLE snap_shot;
    MODULEENTRY32 module;
    LPCREATESNAPSHOT create_snapshot;
    LPMODULEITER module_first, module_next;

    /* XXX: cache this stuff within sigar_t */
    k32_handle = LoadLibrary("kernel32.dll");
    if (!k32_handle) {
        return GetLastError();
    }

    create_snapshot =
        (LPCREATESNAPSHOT)GetProcAddress(k32_handle,
                                         "CreateToolhelp32Snapshot");

    if (!create_snapshot) {
        FreeLibrary(k32_handle);
        return GetLastError();
    }

    module_first =
        (LPMODULEITER)GetProcAddress(k32_handle, "Module32First");

    if (!module_first) {
        FreeLibrary(k32_handle);
        return GetLastError();
    }

    module_next =
        (LPMODULEITER)GetProcAddress(k32_handle, "Module32Next");

    if (!module_next) {
        FreeLibrary(k32_handle);
        return GetLastError();
    }

    snap_shot = create_snapshot(TH32CS_SNAPMODULE, (DWORD)pid);

    if (snap_shot == INVALID_HANDLE_VALUE) {
        FreeLibrary(k32_handle);
        return GetLastError();
    }

    module.dwSize = sizeof(MODULEENTRY32);
    if (!module_first(snap_shot, &module)) {
        return SIGAR_OK;
    }

    do {
        int status =
            procmods->module_getter(procmods->data,
                                    module.szExePath,
                                    strlen(module.szExePath));

        if (status != SIGAR_OK) {
            /* not an error; just stop iterating */
            break;
        }

        module.dwSize = sizeof(MODULEENTRY32);
    } while (module_next(snap_shot, &module));

    FreeLibrary(k32_handle);

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_proc_modules_get(sigar_t *sigar, sigar_pid_t pid,
                                          sigar_proc_modules_t *procmods)
{
    if (sigar->winnt) {
        /* XXX need to use psapi.dll for NT */
        return SIGAR_ENOTIMPL;
    }
    return sigar_proc_modules_get_toolhelp(sigar,
                                           pid,
                                           procmods);
}

int sigar_os_fs_type_get(sigar_file_system_t *fsp)
{
    return fsp->type;
}

SIGAR_DECLARE(int) sigar_file_system_list_get(sigar_t *sigar,
                                              sigar_file_system_list_t *fslist)
{
    sigar_file_system_t *fsp;
    char name[256];
    char *ptr = name;
    /* XXX: hmm, Find{First,Next}Volume not available in my sdk */
    DWORD len = GetLogicalDriveStringsA(sizeof(name), name);

    if (len == 0) {
        return GetLastError();
    }

    sigar_file_system_list_create(fslist);

    while (*ptr) {
        DWORD flags, serialnum=0;
        char fsname[1024];
        UINT type;

        fsname[0] = '\0';

        GetVolumeInformation(ptr, NULL, 0, &serialnum, NULL,
                             &flags, fsname, sizeof(fsname));

        type = GetDriveType(ptr);

        if (!serialnum && (type == DRIVE_FIXED)) {
            ptr += strlen(ptr)+1;
            continue; /* ignore unformatted partitions */
        }

        SIGAR_FILE_SYSTEM_LIST_GROW(fslist);

        fsp = &fslist->data[fslist->number++];

        SIGAR_SSTRCPY(fsp->dir_name, ptr);
        SIGAR_SSTRCPY(fsp->dev_name, ptr);

        switch (type) {
          case DRIVE_FIXED:
            fsp->type = SIGAR_FSTYPE_LOCAL_DISK;
            break;
          case DRIVE_REMOTE:
            fsp->type = SIGAR_FSTYPE_NETWORK;
            break;
          case DRIVE_CDROM:
            fsp->type = SIGAR_FSTYPE_CDROM;
            break;
          case DRIVE_RAMDISK:
            fsp->type = SIGAR_FSTYPE_RAM_DISK;
            break;
          case DRIVE_REMOVABLE:
            /* XXX */
          default:
            fsp->type = SIGAR_FSTYPE_NONE;
            break;
        }

        /* we set fsp->type, just looking up sigar.c:fstype_names[type] */
        sigar_fs_type_get(fsp);

        if (*fsname == '\0') {
            SIGAR_SSTRCPY(fsp->sys_type_name, fsp->type_name);
        }
        else {
            SIGAR_SSTRCPY(fsp->sys_type_name, fsname); /* CDFS, NTFS, etc */
        }

        ptr += strlen(ptr)+1;
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int)
sigar_file_system_usage_get(sigar_t *sigar,
                            const char *dirname,
                            sigar_file_system_usage_t *fsusage)
{
    BOOL retval;
    ULARGE_INTEGER avail, total, free;

    /* prevent dialog box if A:\ drive is empty */
    UINT errmode = SetErrorMode(SEM_FAILCRITICALERRORS);

    retval = GetDiskFreeSpaceEx(dirname,
                                &avail, &total, &free);

    /* restore previous error mode */
    SetErrorMode(errmode);

    if (!retval) {
        return GetLastError();
    }

    fsusage->total = total.QuadPart / 1024;
    fsusage->free  = free.QuadPart / 1024;
    fsusage->avail = avail.QuadPart / 1024;

    fsusage->use_percent = sigar_file_system_usage_calc_used(sigar, fsusage);

    /* XXX */
    fsusage->files = 0;
    fsusage->free_files = 0;

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_cpu_info_list_get(sigar_t *sigar,
                                           sigar_cpu_info_list_t *cpu_infos)
{
    int i, status;
    sigar_cpu_info_t *info;

    sigar_cpu_count(sigar);

    sigar_cpu_info_list_create(cpu_infos);

    info = &cpu_infos->data[cpu_infos->number++];

    status = sigar_cpu_info_get(sigar, info);

    if (status != SIGAR_OK) {
        return status;
    }

    if (sigar->ncpu > 1) {
        for (i=1; i<sigar->ncpu; i++) {
            SIGAR_CPU_INFO_LIST_GROW(cpu_infos);

            memcpy(&cpu_infos->data[cpu_infos->number++],
                   info, sizeof(*info));
        }
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_net_route_list_get(sigar_t *sigar,
                                            sigar_net_route_list_t *routelist)
{
    char *buffer = NULL;
    ULONG bufsize = 0;
    DWORD rc, i;
    MIB_IPFORWARDTABLE *ipt;
    sigar_net_route_t *route;

    if (!sigar->get_ipforward_table) {
        return SIGAR_ENOTIMPL;
    }

    rc = (*(sigar->get_ipforward_table))((PMIB_IPFORWARDTABLE)buffer,
                                         &bufsize, FALSE);
    if (rc != ERROR_INSUFFICIENT_BUFFER) {
        return GetLastError();
    }

    buffer = malloc(bufsize);
    rc = (*(sigar->get_ipforward_table))((PMIB_IPFORWARDTABLE)buffer,
                                         &bufsize, FALSE);
    if (rc != NO_ERROR) {
        free(buffer);
        return GetLastError();
    }

    sigar_net_route_list_create(routelist);
    routelist->size = routelist->number = 0;

    ipt = (MIB_IPFORWARDTABLE *)buffer;

    for (i=0; i<ipt->dwNumEntries; i++) {
        MIB_IPFORWARDROW *ipr = ipt->table + i;

        SIGAR_NET_ROUTE_LIST_GROW(routelist);

        route = &routelist->data[routelist->number++];
        SIGAR_ZERO(route); /* XXX: other fields */

        route->destination = ipr->dwForwardDest;
        route->mask        = ipr->dwForwardMask;
        route->gateway     = ipr->dwForwardNextHop;
    }

    free(buffer);

    return SIGAR_OK;
}

#define IFTYPE_LO  2
#define IFTYPE_ETH 3

static int get_iftype(const char *name, int *type, int *inst)
{
    if (strnEQ(name, "eth", IFTYPE_ETH)) {
        *type = IFTYPE_ETH;
    }
    else if (strnEQ(name, "lo", IFTYPE_LO)) {
        *type = IFTYPE_LO;
    }
    else {
        return EINVAL;
    }

    if (isdigit(*(name + *type))) {
        *inst = atoi(name + *type);
    }
    else {
        return EINVAL;
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int)
sigar_net_interface_stat_get(sigar_t *sigar, const char *name,
                             sigar_net_interface_stat_t *ifstat)
{
    char *buffer = NULL;
    ULONG buf_size = 0;
    DWORD rc, i;
    MIB_IFTABLE *ift;
    MIB_IFROW *ifr;
    DWORD lo=0, eth=0;
    int status, type, inst;

    if ((status = get_iftype(name, &type, &inst)) != SIGAR_OK) {
        return status;
    }

    if (!sigar->get_if_table) {
        return SIGAR_ENOTIMPL;
    }

    rc = (*(sigar->get_if_table))((PMIB_IFTABLE)buffer, &buf_size, FALSE);
    if (rc != ERROR_INSUFFICIENT_BUFFER) {
        return GetLastError();
    }

    buffer = malloc(buf_size);
    rc = (*(sigar->get_if_table))((PMIB_IFTABLE)buffer, &buf_size, FALSE);
    if (rc != NO_ERROR) {
        free(buffer);
        return GetLastError();
    }

    ift = (MIB_IFTABLE *)buffer;

    for (i=0; i<ift->dwNumEntries; i++) {
        ifr = ift->table + i;

        if (!(ifr->dwOperStatus & MIB_IF_OPER_STATUS_OPERATIONAL)) {
            continue;
        }

        if (ifr->dwType == MIB_IF_TYPE_LOOPBACK) {
            if ((type == IFTYPE_LO) && (inst == lo)) {
                break;
            }
            ++lo;
        }
        else if (ifr->dwType == MIB_IF_TYPE_ETHERNET) {
            if ((type == IFTYPE_ETH) && (inst == eth)) {
                break;
            }
            ++eth;
        }

        ifr = NULL;
    }

    if (!ifr) {
        free(buffer);
        return ENOENT;
    }

    ifstat->rx_bytes    = ifr->dwInOctets;
    ifstat->rx_packets  = ifr->dwInUcastPkts + ifr->dwInNUcastPkts; 
    ifstat->rx_errors   = ifr->dwInErrors;
    ifstat->rx_dropped  = ifr->dwInDiscards;
    ifstat->rx_overruns = 0; /*XXX*/
    ifstat->rx_frame    = 0; /*XXX*/

    ifstat->tx_bytes      = ifr->dwOutOctets;
    ifstat->tx_packets    = ifr->dwOutUcastPkts + ifr->dwOutNUcastPkts; 
    ifstat->tx_errors     = ifr->dwOutErrors;
    ifstat->tx_dropped    = ifr->dwOutDiscards;
    ifstat->tx_overruns   = 0; /*XXX*/
    ifstat->tx_collisions = 0; /*XXX*/
    ifstat->tx_carrier    = 0; /*XXX*/

    free(buffer);

    return SIGAR_OK;
}

static int get_iflist(sigar_t *sigar, char *buffer, DWORD buflen, DWORD *bytes)
{
    SOCKET sock = INVALID_SOCKET;
    DWORD rc;
    int status = sigar_wsa_init(sigar);

    if (status != SIGAR_OK) {
        return status;
    }

    sock = WSASocket(PF_INET, SOCK_RAW, AF_INET, 0, 0, 0);
    if (sock == INVALID_SOCKET) {
        return WSAGetLastError();
    }

    rc = WSAIoctl(sock,
                  SIO_GET_INTERFACE_LIST,
                  NULL,
                  0,
                  (void *)buffer,
                  buflen,
                  bytes,
                  0,
                  0);

    status = rc ? WSAGetLastError() : SIGAR_OK;

    closesocket(sock);

    return status;
}

#include <nb30.h>

static void hwaddr_lookup(sigar_net_interface_config_t *ifconfig, int num)
{
    NCB ncb;
    UCHAR rc;
    struct {
        ADAPTER_STATUS status;
        NAME_BUFFER name[30];
    } adapter;

    memset(&ncb, 0, sizeof(ncb));
    ncb.ncb_command = NCBRESET;
    ncb.ncb_lana_num = num;
    Netbios(&ncb);

    memset(&ncb, 0, sizeof(ncb));
    ncb.ncb_command = NCBASTAT;
    ncb.ncb_lana_num = num;

    /*
     * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/netbios/netbios_1l82.asp
     * mdsn docs claim this needs to be padded with spaces and
     * suggest the following silly code:
     * strcpy(ncb.ncb_callname,  "*               ");
     */
    ncb.ncb_callname[0] = '*';
    memset(&ncb.ncb_callname[1], ' ',
           sizeof(ncb.ncb_callname)-1);

    ncb.ncb_callname[sizeof(ncb.ncb_callname)] = '\0';

    ncb.ncb_buffer = (unsigned char *)&adapter;
    ncb.ncb_length = sizeof(adapter);
    if ((rc = Netbios(&ncb)) == 0) {
        sigar_hwaddr_format(ifconfig->hwaddr,
                            adapter.status.adapter_address);
    }
    else {
        sigar_hwaddr_set_null(ifconfig);
    }
}

SIGAR_DECLARE(int)
sigar_net_interface_config_get(sigar_t *sigar,
                               const char *name,
                               sigar_net_interface_config_t *ifconfig)
{
    char buffer[8192];
    DWORD i, num, bytes, inst;
    DWORD lo=0, eth=0;
    int status, type;
    INTERFACE_INFO *if_info = NULL;
    u_long flags;

    /* win32 lacks socket ioctls to query given interface.
     * so we loop through the list to find our made up ifname.
     */
    status = get_iflist(sigar, buffer, sizeof(buffer), &bytes);
    if (status != SIGAR_OK) {
        return status;
    }

    num = bytes / sizeof(INTERFACE_INFO);

    if ((status = get_iftype(name, &type, &inst)) != SIGAR_OK) {
      return status;
    }

    for (i=0; i<num ; i++) {
        if_info = ((INTERFACE_INFO *)buffer) + i;

        if (if_info->iiFlags & IFF_LOOPBACK) {
            if ((type == IFTYPE_LO) && (inst == lo)) {
                break;
            }
            ++lo;
        }
        else {
            if ((type == IFTYPE_ETH) && (inst == eth)) {
                break;
            }
            ++eth;
        }

        if_info = NULL;
    }

    if (!if_info) {
        return ENOENT;
    }

    SIGAR_ZERO(ifconfig);

    SIGAR_SSTRCPY(ifconfig->name, name);

#define if_s_addr(a) \
    ((struct sockaddr_in *)&a)->sin_addr.s_addr

    ifconfig->address   = if_s_addr(if_info->iiAddress);
    ifconfig->broadcast = if_s_addr(if_info->iiBroadcastAddress);
    ifconfig->netmask   = if_s_addr(if_info->iiNetmask);

    flags = if_info->iiFlags;

    if (flags & IFF_UP) {
        ifconfig->flags |= SIGAR_IFF_UP|SIGAR_IFF_RUNNING;
    }
    if (flags & IFF_BROADCAST) {
        ifconfig->flags |= SIGAR_IFF_BROADCAST;
    }
    if (flags & IFF_LOOPBACK) {
        ifconfig->flags |= SIGAR_IFF_LOOPBACK;
        ifconfig->destination = ifconfig->address;
        ifconfig->broadcast = 0;
        sigar_hwaddr_set_null(ifconfig);
    }
    else {
        hwaddr_lookup(ifconfig, i);
    }
    if (flags & IFF_POINTTOPOINT) {
        ifconfig->flags |= SIGAR_IFF_POINTOPOINT;
    }
    if (flags & IFF_MULTICAST) {
        ifconfig->flags |= SIGAR_IFF_MULTICAST;
    }

    return SIGAR_OK;
}

/*
 * win32 interface list does not include a name.
 * and the name from GetIfList() is the name of card
 * including vendor name, etc.  so we use 'eth' for ethernet
 * interfaces and 'lo' for loopback.
 */

#define ETH "eth"
#define LO  "lo"

SIGAR_DECLARE(int)
sigar_net_interface_list_get(sigar_t *sigar,
                             sigar_net_interface_list_t *iflist)
{
    char eth[56], lo[56];
    int ethcnt=0, locnt=0;
    char buffer[8192];
    DWORD i, num, bytes;
    int status;
    
    status = get_iflist(sigar, buffer, sizeof(buffer), &bytes);
    if (status != SIGAR_OK) {
        return status;
    }

    num = bytes / sizeof(INTERFACE_INFO);

    iflist->number = 0;
    iflist->size = num;
    iflist->data =
        malloc(sizeof(*(iflist->data)) * iflist->size);

    for (i=0; i<num ; i++) {
        INTERFACE_INFO *if_info = ((INTERFACE_INFO *)buffer) + i;
        char *name;

        if (if_info->iiFlags & IFF_LOOPBACK) {
            sprintf(lo, LO "%d", locnt++);
            name = strdup(lo);
        }
        else {
            /* XXX: assuming ethernet here */
            sprintf(eth, ETH "%d", ethcnt++);
            name = strdup(eth);
        }

        iflist->data[iflist->number++] = name;
    }

    return SIGAR_OK;
}

static void ip_format(char *buffer, int buflen, UINT addr)
{
    UINT ip = htonl(addr);

    sprintf(buffer, "%d.%d.%d.%d",
            ((ip >> 24) & 0xFF),
            ((ip >> 16) & 0xFF),
            ((ip >> 8) & 0xFF),
            ((ip) & 0xFF));
}

static int net_conn_get_tcp(sigar_t *sigar,
                            sigar_net_connection_list_t *connlist,
                            int flags)
{
    int status;
    DWORD rc, size, i;
    PMIB_TCPTABLE tcp;

    size = 0;
    rc = sigar->get_tcp_table(NULL, &size, FALSE);
    if (rc != ERROR_INSUFFICIENT_BUFFER) {
        return GetLastError();
    }
    tcp = malloc(size);
    rc = sigar->get_tcp_table(tcp, &size, FALSE);
    if (rc) {
        free(tcp);
        return GetLastError();
    }

    for (i = 0; i < tcp->dwNumEntries; i++) {
        sigar_net_connection_t conn;
        DWORD state = tcp->table[i].dwState;

        if (flags & SIGAR_NETCONN_SERVER) {
            if (state != MIB_TCP_STATE_LISTEN) {
                continue;
            }
        }
        else if (flags & SIGAR_NETCONN_CLIENT) {
            if (state == MIB_TCP_STATE_LISTEN) {
                continue;
            }
        }

        conn.local_port  = htons((WORD)tcp->table[i].dwLocalPort);
        conn.remote_port = htons((WORD)tcp->table[i].dwRemotePort);

        conn.type = SIGAR_NETCONN_TCP;

        ip_format(conn.local_address,
                  sizeof(conn.local_address),
                  tcp->table[i].dwLocalAddr);

        ip_format(conn.remote_address,
                  sizeof(conn.remote_address),
                  tcp->table[i].dwRemoteAddr);

        SIGAR_NET_CONNLIST_GROW(connlist);
        memcpy(&connlist->data[connlist->number++],
               &conn, sizeof(conn));
    }

    free(tcp);
    return SIGAR_OK;
}

static int net_conn_get_udp(sigar_t *sigar,
                            sigar_net_connection_list_t *connlist,
                            int flags)
{
    int status;
    DWORD rc, size, i;
    PMIB_UDPTABLE udp;

    size = 0;
    rc = sigar->get_udp_table(NULL, &size, FALSE);
    if (rc != ERROR_INSUFFICIENT_BUFFER) {
        return GetLastError();
    }
    udp = malloc(size);
    rc = sigar->get_udp_table(udp, &size, FALSE);
    if (rc) {
        free(udp);
        return GetLastError();
    }

    for (i = 0; i < udp->dwNumEntries; i++) {
        sigar_net_connection_t conn;

        if (!((conn.remote_port && (flags & SIGAR_NETCONN_CLIENT)) ||
              (!conn.remote_port && (flags & SIGAR_NETCONN_SERVER))))
        {
            continue;
        }

        conn.local_port  = htons((WORD)udp->table[i].dwLocalPort);
        conn.remote_port = conn.local_port;

        conn.type = SIGAR_NETCONN_UDP;

        ip_format(conn.local_address,
                  sizeof(conn.local_address),
                  udp->table[i].dwLocalAddr);

        ip_format(conn.remote_address,
                  sizeof(conn.remote_address),
                  udp->table[i].dwLocalAddr);

        SIGAR_NET_CONNLIST_GROW(connlist);
        memcpy(&connlist->data[connlist->number++],
               &conn, sizeof(conn));
    }

    free(udp);
    return SIGAR_OK;
}

SIGAR_DECLARE(int)
sigar_net_connection_list_get(sigar_t *sigar,
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

SIGAR_DECLARE(int) sigar_proc_port_get(sigar_t *sigar,
                                       unsigned long port,
                                       sigar_pid_t *pid)
{
    int status;
    DWORD rc, i;
    PMIB_TCPEXTABLE tcp;

    if (!sigar->get_tcpx_table) {
        return SIGAR_ENOTIMPL;
    }

    rc = sigar->get_tcpx_table(&tcp, FALSE, GetProcessHeap(),
                               2, 2);

    if (rc) {
        return GetLastError();
    }

    for (i=0; i<tcp->dwNumEntries; i++) {
        if (tcp->table[i].dwState != MIB_TCP_STATE_LISTEN) {
            continue;
        }

        if (htons((WORD)tcp->table[i].dwLocalPort) != port) {
            continue;
        }

        *pid = tcp->table[i].dwProcessId;

        return SIGAR_OK;
    }

    return ENOENT;
}
