/*
 * functions for getting info from the Process Environment Block
 */
#define UNICODE
#define _UNICODE

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"

#define PAGE_START    0x00020000
#define CWD_OFFSET    PAGE_START + 0x0290
#define PATH_OFFSET   PAGE_START + 0x0498
#define START_ADDRESS PAGE_START + 0x0498

static int sigar_peb_get(sigar_t *sigar, HANDLE proc, DWORD *base)
{
    MEMORY_BASIC_INFORMATION mbi;
    DWORD bytes;

    if (!sigar->peb) {
        sigar->peb = malloc(sigar->pagesize);
    }

    if (!VirtualQueryEx(proc, (char*)START_ADDRESS, &mbi, sizeof(mbi))) {
        return GetLastError();
    }

    if (!ReadProcessMemory(proc, mbi.BaseAddress, sigar->peb,
                           sigar->pagesize, &bytes))
    {
        return GetLastError();
    }

    *base = (DWORD)mbi.BaseAddress;

    return SIGAR_OK;
}

#define SKIP_NULL(scratch) \
    if (*scratch == '\0') scratch += sizeof(WCHAR)

#define PEB_START(scratch, base, offset) \
    scratch = sigar->peb + ((DWORD)offset - base)

//point scratch to next string (assumes PEB_FIRST)
#define PEB_NEXT(scratch) \
    scratch = scratch + (wcslen((LPWSTR)scratch) + 1) * sizeof(WCHAR); \
    SKIP_NULL(scratch)

int sigar_proc_exe_peb_get(sigar_t *sigar, HANDLE proc,
                           sigar_proc_exe_t *procexe)
{
    int status;
    LPBYTE scratch;
    DWORD base;
    WCHAR buf[MAX_PATH];

    if ((status = sigar_peb_get(sigar, proc, &base)) != SIGAR_OK) {
        return status;
    }

    PEB_START(scratch, base, CWD_OFFSET);

    wcsncpy(buf, (LPWSTR)scratch, MAX_PATH);
    buf[MAX_PATH-1] = L'\0';

    SIGAR_W2A(buf, procexe->cwd, sizeof(procexe->cwd));

    PEB_START(scratch, base, PATH_OFFSET);

    PEB_NEXT(scratch); //skip PATH

    wcsncpy(buf, (LPWSTR)scratch, MAX_PATH);
    buf[MAX_PATH-1] = L'\0';

    SIGAR_W2A(buf, procexe->name, sizeof(procexe->name));

    return SIGAR_OK;
}

int sigar_proc_cmdline_get(sigar_t *sigar, HANDLE proc, char *cmdline)
{
    int status;
    LPBYTE scratch;
    DWORD base;
    WCHAR buf[SIGAR_CMDLINE_MAX];

    if ((status = sigar_peb_get(sigar, proc, &base)) != SIGAR_OK) {
        return status;
    }

    PEB_START(scratch, base, PATH_OFFSET);

    PEB_NEXT(scratch); //skip PATH

    PEB_NEXT(scratch); //skip exe name

    wcsncpy(buf, (LPWSTR)scratch, SIGAR_CMDLINE_MAX);
    buf[SIGAR_CMDLINE_MAX-1] = L'\0';

    SIGAR_W2A(buf, cmdline, SIGAR_CMDLINE_MAX);

    return SIGAR_OK;
}
