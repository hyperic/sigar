/*
 * functions for getting info from the Process Environment Block
 */
#define UNICODE
#define _UNICODE

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"

#define START_ADDRESS (LPVOID)0x00020498

static int sigar_peb_get(sigar_t *sigar, HANDLE proc, DWORD *base)
{
    MEMORY_BASIC_INFORMATION mbi;
    DWORD bytes;

    if (!sigar->peb) {
        sigar->peb = malloc(sigar->pagesize);
    }

    if (!VirtualQueryEx(proc, START_ADDRESS, &mbi, sizeof(mbi))) {
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

//point scratch to env block
#define PEB_FIRST(scratch, base) \
    scratch = sigar->peb + ((DWORD)START_ADDRESS - base)

//point scratch to next string (assumes PEB_FIRST)
#define PEB_NEXT(scratch) \
    scratch = scratch + (wcslen((LPWSTR)scratch) + 1) * sizeof(WCHAR)

int sigar_proc_exe_name_get(sigar_t *sigar, HANDLE proc, char *name)
{
    int status;
    LPBYTE scratch;
    DWORD base;
    WCHAR buf[MAX_PATH];

    if ((status = sigar_peb_get(sigar, proc, &base)) != SIGAR_OK) {
        return status;
    }

    //skip env PATH
    PEB_FIRST(scratch, base);

    PEB_NEXT(scratch);

    //seems common, reason unknown.
    if (*scratch == '\0') {
        scratch += sizeof(WCHAR);
    }

    wcsncpy(buf, (LPWSTR)scratch, MAX_PATH);
    buf[MAX_PATH-1] = L'\0';

    SIGAR_W2A(buf, name, MAX_PATH);

    return SIGAR_OK;
}

int sigar_proc_cmdline_get(sigar_t *sigar, HANDLE proc, char *cmdline)
{
    int status;
    LPBYTE scratch;
    DWORD base;
    WCHAR buf[MAX_PATH];

    if ((status = sigar_peb_get(sigar, proc, &base)) != SIGAR_OK) {
        return status;
    }

    //skip env block
    PEB_FIRST(scratch, base);

    PEB_NEXT(scratch);
    //seems common, reason unknown.
    if (*scratch == '\0') {
        scratch += sizeof(WCHAR);
    }

    PEB_NEXT(scratch);
    if (*scratch == '\0') {
        scratch += sizeof(WCHAR);
    }

    wcsncpy(buf, (LPWSTR)scratch, MAX_PATH);
    buf[MAX_PATH-1] = L'\0';

    SIGAR_W2A(buf, cmdline, MAX_PATH);

    return SIGAR_OK;
}
