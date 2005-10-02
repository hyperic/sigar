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

/* from iptypes.h not in vs6.0 */
#define MAX_ADAPTER_DESCRIPTION_LENGTH  128
#define MAX_ADAPTER_NAME_LENGTH         256
#define MAX_ADAPTER_ADDRESS_LENGTH      8
#define MAX_HOSTNAME_LEN                128
#define MAX_DOMAIN_NAME_LEN             128
#define MAX_SCOPE_ID_LEN                256

typedef struct {
    char String[4 * 4];
} IP_ADDRESS_STRING, *PIP_ADDRESS_STRING, IP_MASK_STRING, *PIP_MASK_STRING;

typedef struct _IP_ADDR_STRING {
    struct _IP_ADDR_STRING* Next;
    IP_ADDRESS_STRING IpAddress;
    IP_MASK_STRING IpMask;
    DWORD Context;
} IP_ADDR_STRING, *PIP_ADDR_STRING;

typedef struct {
    char HostName[MAX_HOSTNAME_LEN + 4];
    char DomainName[MAX_DOMAIN_NAME_LEN + 4];
    PIP_ADDR_STRING CurrentDnsServer;
    IP_ADDR_STRING DnsServerList;
    UINT NodeType;
    char ScopeId[MAX_SCOPE_ID_LEN + 4];
    UINT EnableRouting;
    UINT EnableProxy;
    UINT EnableDns;
} FIXED_INFO, *PFIXED_INFO;

typedef struct _IP_ADAPTER_INFO {
    struct _IP_ADAPTER_INFO* Next;
    DWORD ComboIndex;
    char AdapterName[MAX_ADAPTER_NAME_LENGTH + 4];
    char Description[MAX_ADAPTER_DESCRIPTION_LENGTH + 4];
    UINT AddressLength;
    BYTE Address[MAX_ADAPTER_ADDRESS_LENGTH];
    DWORD Index;
    UINT Type;
    UINT DhcpEnabled;
    PIP_ADDR_STRING CurrentIpAddress;
    IP_ADDR_STRING IpAddressList;
    IP_ADDR_STRING GatewayList;
    IP_ADDR_STRING DhcpServer;
    BOOL HaveWins;
    IP_ADDR_STRING PrimaryWinsServer;
    IP_ADDR_STRING SecondaryWinsServer;
    time_t LeaseObtained;
    time_t LeaseExpires;
} IP_ADAPTER_INFO, *PIP_ADAPTER_INFO;

/* end iptypes.h */

/* from wtsapi32.h not in vs6.0 */
typedef enum {
    WTSInitialProgram,
    WTSApplicationName,
    WTSWorkingDirectory,
    WTSOEMId,
    WTSSessionId,
    WTSUserName,
    WTSWinStationName,
    WTSDomainName,
    WTSConnectState,
    WTSClientBuildNumber,
    WTSClientName,
    WTSClientDirectory,
    WTSClientProductId,
    WTSClientHardwareId,
    WTSClientAddress,
    WTSClientDisplay,
    WTSClientProtocolType,
} WTS_INFO_CLASS;

typedef enum _WTS_CONNECTSTATE_CLASS {
    WTSActive,
    WTSConnected,
    WTSConnectQuery,
    WTSShadow,
    WTSDisconnected,
    WTSIdle,
    WTSListen,
    WTSReset,
    WTSDown,
    WTSInit
} WTS_CONNECTSTATE_CLASS;

#define WTS_PROTOCOL_TYPE_CONSOLE 0
#define WTS_PROTOCOL_TYPE_ICA     1
#define WTS_PROTOCOL_TYPE_RDP     2

typedef struct _WTS_SESSION_INFO {
    DWORD SessionId;
    LPTSTR pWinStationName;
    DWORD State;
} WTS_SESSION_INFO, *PWTS_SESSION_INFO;

typedef struct _WTS_PROCESS_INFO {
    DWORD SessionId;
    DWORD ProcessId;
    LPSTR pProcessName;
    PSID pUserSid;
} WTS_PROCESS_INFO, *PWTS_PROCESS_INFO;

typedef struct _WTS_CLIENT_ADDRESS {
    DWORD AddressFamily;
    BYTE Address[20];
} WTS_CLIENT_ADDRESS, *PWTS_CLIENT_ADDRESS;

/* the WINSTATION_INFO stuff here is undocumented
 * got the howto from google groups:
 * http://redirx.com/?31gy
 */
typedef enum _WINSTATION_INFO_CLASS {
    WinStationInformation = 8
} WINSTATION_INFO_CLASS;

typedef struct _WINSTATION_INFO {
    BYTE Reserved1[72];
    ULONG SessionId;
    BYTE Reserved2[4];
    FILETIME ConnectTime;
    FILETIME DisconnectTime;
    FILETIME LastInputTime;
    FILETIME LoginTime;
    BYTE Reserved3[1096];
    FILETIME CurrentTime;
} WINSTATION_INFO, *PWINSTATION_INFO;

/* end wtsapi32.h */

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

typedef BOOL (CALLBACK *LPCONVERTSTRINGSID)(LPCSTR, PSID *);

typedef DWORD (CALLBACK *LPGETIPFORWARDTABLE)(PMIB_IPFORWARDTABLE, PULONG, BOOL);

typedef DWORD (CALLBACK *LPGETIFTABLE)(PMIB_IFTABLE, PULONG, BOOL);

typedef DWORD (CALLBACK *LPGETTCPTABLE)(PMIB_TCPTABLE, PDWORD, BOOL);

typedef DWORD (CALLBACK *LPGETUDPTABLE)(PMIB_UDPTABLE, PDWORD, BOOL);

typedef DWORD (CALLBACK *LPGETTCPEXTABLE)(PMIB_TCPEXTABLE *, BOOL, HANDLE,
                                          DWORD, DWORD);

typedef DWORD (CALLBACK *LPGETUDPEXTABLE)(PMIB_UDPEXTABLE *, BOOL, HANDLE,
                                          DWORD, DWORD);

typedef DWORD (CALLBACK *LPNETPARAMS)(PFIXED_INFO, PULONG);

typedef DWORD (CALLBACK *LPADAPTERSINFO)(PIP_ADAPTER_INFO, PULONG);

typedef DWORD (CALLBACK *LPSYSINFO)(DWORD, PVOID, ULONG, PULONG);

typedef BOOL (CALLBACK *LPENUMMODULES)(HANDLE, HMODULE*,
                                       DWORD, LPDWORD);

typedef DWORD (CALLBACK *LPGETMODULENAME)(HANDLE, HMODULE,
                                          LPTSTR, DWORD);

typedef BOOLEAN (CALLBACK *LPSTATIONQUERYINFO)(HANDLE,
                                               ULONG,
                                               WINSTATION_INFO_CLASS,
                                               PVOID, ULONG, PULONG);

typedef BOOL (CALLBACK *LPWTSENUMERATESESSIONS)(HANDLE,
                                                DWORD,
                                                DWORD,
                                                PWTS_SESSION_INFO *,
                                                DWORD *);

typedef void (CALLBACK *LPWTSFREEMEMORY)(PVOID);

typedef BOOL (CALLBACK *LPWTSQUERYSESSION)(HANDLE,
                                           DWORD,
                                           WTS_INFO_CLASS,
                                           LPSTR *, DWORD *);

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
    HINSTANCE adv_handle;
    HINSTANCE ip_handle;
    HINSTANCE nt_handle;
    HINSTANCE ps_handle;
    HINSTANCE wts_handle;
    HINSTANCE sta_handle;
    LPCONVERTSTRINGSID convert_string_sid;
    LPGETIFTABLE get_if_table;
    LPGETIPFORWARDTABLE get_ipforward_table;
    LPGETTCPTABLE get_tcp_table;
    LPGETTCPEXTABLE get_tcpx_table;
    LPGETUDPTABLE get_udp_table;
    LPGETUDPEXTABLE get_udpx_table;
    LPNETPARAMS get_net_params;
    LPADAPTERSINFO get_adapters_info;
    LPSYSINFO get_ntsys_info;
    LPENUMMODULES enum_modules;
    LPGETMODULENAME get_module_name;
    sigar_win32_pinfo_t pinfo;
    LPSTATIONQUERYINFO query_station;
    LPWTSENUMERATESESSIONS wts_enum_sessions;
    LPWTSFREEMEMORY wts_free;
    LPWTSQUERYSESSION wts_query_session;
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
