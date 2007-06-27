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

/*
 * functions for getting info from the Process Environment Block
 */
#define UNICODE
#define _UNICODE

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"
#include <shellapi.h>

#define PAGE_START    0x00020000
#define CWD_OFFSET    PAGE_START + 0x0290
#define PATH_OFFSET   PAGE_START + 0x0498
#define START_ADDRESS PAGE_START + 0x0498

static int sigar_peb_get(sigar_t *sigar, HANDLE proc, DWORD *base)
{
    MEMORY_BASIC_INFORMATION mbi;
    DWORD bytes;
    SIZE_T size = sigar->pagesize;

    if (!sigar->peb) {
        sigar->peb = malloc(sigar->pagesize*2);
    }

    if (!VirtualQueryEx(proc, (char*)START_ADDRESS, &mbi, sizeof(mbi))) {
        return GetLastError();
    }

    if (mbi.RegionSize > sigar->pagesize) {
        /* in the event args crosses the first page boundry.
         * seen with WebSphere.
         */
        size *= 2;
    }

    if (!ReadProcessMemory(proc, mbi.BaseAddress, sigar->peb,
                           size, &bytes))
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

    /* XXX seen on non-english windows, random leading char */
    if (*(scratch + sizeof(WCHAR)) != L':') {
        scratch += sizeof(WCHAR);
    }

    wcsncpy(buf, (LPWSTR)scratch, MAX_PATH);
    buf[MAX_PATH-1] = L'\0';

    SIGAR_W2A(buf, procexe->name, sizeof(procexe->name));

    return SIGAR_OK;
}

int sigar_parse_proc_args(sigar_t *sigar, WCHAR *buf,
                          sigar_proc_args_t *procargs)
{
    char arg[SIGAR_CMDLINE_MAX];
    LPWSTR *args;
    int num, i;

    if (!buf) {
        buf = GetCommandLine();
    }

    args = CommandLineToArgvW(buf, &num);

    if (args == NULL) {
        return SIGAR_OK;
    }

    for (i=0; i<num; i++) {
        SIGAR_W2A(args[i], arg, SIGAR_CMDLINE_MAX);
        SIGAR_PROC_ARGS_GROW(procargs);
        procargs->data[procargs->number++] = sigar_strdup(arg);
    }

    GlobalFree(args);

    return SIGAR_OK;
}

int sigar_proc_args_peb_get(sigar_t *sigar, HANDLE proc,
                            sigar_proc_args_t *procargs)
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

    return sigar_parse_proc_args(sigar, buf, procargs);
}
