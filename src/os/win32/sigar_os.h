#ifndef SIGAR_OS_H
#define SIGAR_OS_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winreg.h>
#include <winperf.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <stddef.h>
#include <sys/types.h>
#include <malloc.h>
#include <stdio.h>
#include <errno.h>
#include <tlhelp32.h>

#define INT64_C(val) val##i64

/* see apr/include/arch/win32/atime.h */
#define EPOCH_DELTA INT64_C(11644473600000000)

#define SIGAR_CMDLINE_MAX 4096

static __inline sigar_uint64_t FileTimeToTime(FILETIME *ft)
{
    sigar_uint64_t time;
    time = ft->dwHighDateTime;
    time = time << 32;
    time |= ft->dwLowDateTime;
    time /= 10;
    time -= EPOCH_DELTA;
    return time;
}

/* XXX: support CP_UTF8 ? */

#define SIGAR_A2W(lpa, lpw, bytes) \
    (lpw[0] = 0, MultiByteToWideChar(CP_ACP, 0, \
                                     lpa, -1, lpw, (bytes/sizeof(WCHAR))))

#define SIGAR_W2A(lpw, lpa, chars) \
    (lpa[0] = '\0', WideCharToMultiByte(CP_ACP, 0, \
                                        lpw, -1, (LPSTR)lpa, chars, \
                                        NULL, NULL))

#include <iprtrmib.h>

/* undocumented structures */
typedef struct {
    DWORD   dwState;
    DWORD   dwLocalAddr;
    DWORD   dwLocalPort;
    DWORD   dwRemoteAddr;
    DWORD   dwRemotePort;
    DWORD   dwProcessId;
} MIB_TCPEXROW, *PMIB_TCPEXROW;

typedef struct {
    DWORD dwNumEntries;
    MIB_TCPEXROW table[ANY_SIZE];
} MIB_TCPEXTABLE, *PMIB_TCPEXTABLE;

typedef struct {
    DWORD dwLocalAddr;
    DWORD dwLocalPort;
    DWORD dwProcessId;
} MIB_UDPEXROW, *PMIB_UDPEXROW;

typedef struct {
    DWORD dwNumEntries;
    MIB_UDPEXROW table[ANY_SIZE];
} MIB_UDPEXTABLE, *PMIB_UDPEXTABLE;

/* end undocumented structures */

typedef DWORD (CALLBACK *LPGETIPFORWARDTABLE)(PMIB_IPFORWARDTABLE, PULONG, BOOL);

typedef DWORD (CALLBACK *LPGETIFTABLE)(PMIB_IFTABLE, PULONG, BOOL);

typedef DWORD (CALLBACK *LPGETTCPTABLE)(PMIB_TCPTABLE, PDWORD, BOOL);

typedef DWORD (CALLBACK *LPGETUDPTABLE)(PMIB_UDPTABLE, PDWORD, BOOL);

typedef DWORD (CALLBACK *LPGETTCPEXTABLE)(PMIB_TCPEXTABLE *, BOOL, HANDLE,
                                          DWORD, DWORD);

typedef DWORD (CALLBACK *LPSYSINFO)(DWORD, PVOID, ULONG, PULONG);

/* no longer in the standard header files */
typedef struct {
    LARGE_INTEGER IdleTime;
    LARGE_INTEGER KernelTime;
    LARGE_INTEGER UserTime;
    LARGE_INTEGER Reserved1[2];
    ULONG Reserved2;
} SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION;

#define SystemProcessorPerformanceInformation 8

typedef struct {
    sigar_pid_t pid;
    int ppid;
    int priority;
    time_t mtime;
    sigar_uint64_t vsize;
    sigar_uint64_t size;
    char name[SIGAR_PROC_NAME_LEN];
    char state;
    sigar_uint64_t handles;
} sigar_win32_pinfo_t;

struct sigar_t {
    SIGAR_T_BASE;
    char *machine;
    int using_wide;
    long pagesize;
    HKEY handle;
    char *perfbuf;
    DWORD perfbuf_size;
    HINSTANCE ip_handle;
    HINSTANCE nt_handle;
    LPGETIFTABLE get_if_table;
    LPGETIPFORWARDTABLE get_ipforward_table;
    LPGETTCPTABLE get_tcp_table;
    LPGETTCPEXTABLE get_tcpx_table;
    LPGETUDPTABLE get_udp_table;
    LPSYSINFO get_ntsys_info;
    sigar_win32_pinfo_t pinfo;
    WORD ws_version;
    int ws_error;
    LPBYTE peb; //scratch pad for getting peb info
    int ht_enabled;
    int lcpu; //number of logical cpus
    int winnt;
};

int sigar_wsa_init(sigar_t *sigar);

int sigar_proc_exe_peb_get(sigar_t *sigar, HANDLE proc,
                           sigar_proc_exe_t *procexe);

int sigar_proc_args_peb_get(sigar_t *sigar, HANDLE proc,
                            sigar_proc_args_t *procargs);

int sigar_parse_proc_args(sigar_t *sigar, WCHAR *buf,
                          sigar_proc_args_t *procargs);

unsigned int sigar_cpu_count(sigar_t *sigar);

int sigar_cpu_info_get(sigar_t *sigar, sigar_cpu_info_t *info);

#define SIGAR_NO_SUCH_PROCESS (SIGAR_OS_START_ERROR+1)

#endif /* SIGAR_OS_H */
