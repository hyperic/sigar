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

void dllmod_init_ntdll(sigar_t *sigar);

#define sigar_NtQueryInformationProcess \
    sigar->ntdll.query_proc_info.func

static int sigar_pbi_get(sigar_t *sigar, HANDLE proc, PEB *peb)
{
    int status;
    PROCESS_BASIC_INFORMATION pbi;
    DWORD size=sizeof(pbi);

    dllmod_init_ntdll(sigar);

    if (!sigar_NtQueryInformationProcess) {
        return SIGAR_ENOTIMPL;
    }

    SIGAR_ZERO(&pbi);
    status =
        sigar_NtQueryInformationProcess(proc,
                                        ProcessBasicInformation,
                                        &pbi,
                                        size, NULL);
    if (status != ERROR_SUCCESS) {
        return status;
    }

    if (!pbi.PebBaseAddress) {
        return !SIGAR_OK;
    }

    size = sizeof(*peb);

    if (ReadProcessMemory(proc, pbi.PebBaseAddress, peb, size, NULL)) {
        return SIGAR_OK;
    }
    else {
        return GetLastError();
    }
}

static int sigar_rtl_get(sigar_t *sigar, HANDLE proc,
                         RTL_USER_PROCESS_PARAMETERS *rtl)
{
    PEB peb;
    int status = sigar_pbi_get(sigar, proc, &peb);
    DWORD size=sizeof(*rtl);

    if (status != SIGAR_OK) {
        return status;
    }

    if (ReadProcessMemory(proc, peb.ProcessParameters, rtl, size, NULL)) {
        return SIGAR_OK;
    }
    else {
        return GetLastError();
    }
}

#define rtl_bufsize(buf, uc) \
    ((sizeof(buf) < uc.Length) ? sizeof(buf) : uc.Length)

int sigar_proc_exe_peb_get(sigar_t *sigar, HANDLE proc,
                           sigar_proc_exe_t *procexe)
{
    int status;
    WCHAR buf[MAX_PATH+1];
    RTL_USER_PROCESS_PARAMETERS rtl;
    DWORD size;

    if ((status = sigar_rtl_get(sigar, proc, &rtl)) != SIGAR_OK) {
        return status;
    }

    size = rtl_bufsize(buf, rtl.ImagePathName);
    memset(buf, '\0', sizeof(buf));

    if ((size > 0) &&
        ReadProcessMemory(proc, rtl.ImagePathName.Buffer, buf, size, NULL))
    {
        SIGAR_W2A(buf, procexe->name, sizeof(procexe->name));
    }
    else {
        procexe->name[0] = '\0';
    }

    size = rtl_bufsize(buf, rtl.CurrentDirectoryName);
    memset(buf, '\0', sizeof(buf));

    if ((size > 0) &&
        ReadProcessMemory(proc, rtl.CurrentDirectoryName.Buffer, buf, size, NULL))
    {
        SIGAR_W2A(buf, procexe->cwd, sizeof(procexe->cwd));
    }
    else {
        procexe->cwd[0] = '\0';
    }

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
    WCHAR buf[SIGAR_CMDLINE_MAX];
    RTL_USER_PROCESS_PARAMETERS rtl;
    DWORD size;

    if ((status = sigar_rtl_get(sigar, proc, &rtl)) != SIGAR_OK) {
        return status;
    }

    size = rtl_bufsize(buf, rtl.CommandLine);
    memset(buf, '\0', sizeof(buf));
            
    if ((size > 0) &&
        ReadProcessMemory(proc, rtl.CommandLine.Buffer, buf, size, NULL))
    {
        return sigar_parse_proc_args(sigar, buf, procargs);
    }
    else {
        return SIGAR_OK;
    }
}

int sigar_proc_env_peb_get(sigar_t *sigar, HANDLE proc,
                           WCHAR *buf, DWORD size)
{
    int status;
    RTL_USER_PROCESS_PARAMETERS rtl;
    MEMORY_BASIC_INFORMATION info;

    if ((status = sigar_rtl_get(sigar, proc, &rtl)) != SIGAR_OK) {
        return status;
    }

    memset(buf, '\0', size);
    /* -2 to ensure \0\0 terminator */
    size -= 2;

    if (VirtualQueryEx(proc, rtl.Environment, &info, sizeof(info))) {
        if (size > info.RegionSize) {
            /* ReadProcessMemory beyond region would fail */
            size = info.RegionSize;
        }
    }

    if (ReadProcessMemory(proc, rtl.Environment, buf, size, NULL)) {
        return SIGAR_OK;
    }
    else {
        return GetLastError();
    }
}
