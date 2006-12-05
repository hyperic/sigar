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

#include "vmcontrol_wrapper.h"

#ifdef VMCONTROL_WRAPPER_SUPPORTED

#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#endif

#include <stdio.h>

#ifdef WIN32
#define DL_LOAD(lib)  LoadLibrary(lib)
#define DL_CLOSE(h)   FreeLibrary(h)
#define DL_SYM        GetProcAddress
#define DL_ERR        GetLastError()
#define DL_ENOENT     ERROR_FILE_NOT_FOUND
#else
#define DL_LOAD(lib)  dlopen(lib, RTLD_LAZY)
#define DL_CLOSE(h)   dlclose(h)
#define DL_SYM        dlsym
#define DL_ERR        errno
#define DL_ENOENT     ENOENT
#endif

typedef struct {
    const char *name;
    unsigned long offset;
    const char *alias;
} vmcontrol_entry_t;

#define OffsetOf(structure, field) \
    (unsigned long)(&((structure *)NULL)->field)

#define VMCONTROL_ENTRY(name) \
    { #name, OffsetOf(vmcontrol_wrapper_api_t, x##name), NULL }

#define VMCONTROL_ENTRY_ALIAS(name, alias) \
    { #name, OffsetOf(vmcontrol_wrapper_api_t, x##name), alias }

static vmcontrol_entry_t vmcontrol_entries[] = {
    VMCONTROL_ENTRY(VMControl_ConnectParamsDestroy),
    VMCONTROL_ENTRY(VMControl_ConnectParamsNew),
    VMCONTROL_ENTRY(VMControl_Init),
    VMCONTROL_ENTRY(VMControl_MKSSaveScreenshot),
    VMCONTROL_ENTRY(VMControl_ServerConnectEx),
    VMCONTROL_ENTRY(VMControl_ServerDestroy),
    VMCONTROL_ENTRY(VMControl_ServerDisconnect),
    VMCONTROL_ENTRY(VMControl_ServerEnumerate),
    VMCONTROL_ENTRY(VMControl_ServerExec),
    VMCONTROL_ENTRY(VMControl_ServerGetLastError),
    VMCONTROL_ENTRY(VMControl_ServerGetResource),
    VMCONTROL_ENTRY(VMControl_ServerIsConnected),
    VMCONTROL_ENTRY(VMControl_ServerIsRegistered),
    VMCONTROL_ENTRY(VMControl_ServerNewEx),
    VMCONTROL_ENTRY(VMControl_VMConnectEx),
    VMCONTROL_ENTRY(VMControl_VMCreateSnapshot),
    VMCONTROL_ENTRY(VMControl_VMDestroy),
    VMCONTROL_ENTRY(VMControl_VMDeviceConnect),
    VMCONTROL_ENTRY(VMControl_VMDeviceDisconnect),
    VMCONTROL_ENTRY(VMControl_VMDeviceIsConnected),
    VMCONTROL_ENTRY(VMControl_VMDisconnect),
    VMCONTROL_ENTRY(VMControl_VMGetCapabilities),
    VMCONTROL_ENTRY(VMControl_VMGetConfig),
    VMCONTROL_ENTRY(VMControl_VMGetConfigFileName),
    VMCONTROL_ENTRY(VMControl_VMGetExecutionState),
    VMCONTROL_ENTRY(VMControl_VMGetGuestInfo),
    VMCONTROL_ENTRY(VMControl_VMGetHeartbeat),
    VMCONTROL_ENTRY(VMControl_VMGetId),
    VMCONTROL_ENTRY(VMControl_VMGetLastError),
    VMCONTROL_ENTRY(VMControl_VMGetPid),
    VMCONTROL_ENTRY(VMControl_VMGetProductInfo),
    VMCONTROL_ENTRY(VMControl_VMGetRemoteConnections),
    VMCONTROL_ENTRY(VMControl_VMGetResource),
    VMCONTROL_ENTRY(VMControl_VMGetRunAsUser),
    VMCONTROL_ENTRY(VMControl_VMGetUptime),
    VMCONTROL_ENTRY(VMControl_VMHasSnapshot),
    VMCONTROL_ENTRY(VMControl_VMInit),
    VMCONTROL_ENTRY(VMControl_VMIsConnected),
    VMCONTROL_ENTRY(VMControl_VMNewEx),
    VMCONTROL_ENTRY_ALIAS(VMControl_VMRemoveAllSnapshots,
                          "VMControl_VMDeleteSnapshot"),
    VMCONTROL_ENTRY_ALIAS(VMControl_VMRevertToSnapshot,
                          "VMControl_VMRevertSnapshot"),
    VMCONTROL_ENTRY(VMControl_VMSetConfig),
    VMCONTROL_ENTRY(VMControl_VMSetGuestInfo),
    VMCONTROL_ENTRY(VMControl_VMStart),
    VMCONTROL_ENTRY(VMControl_VMStopOrReset),
    VMCONTROL_ENTRY(VMControl_VMSuspendToDisk),
    VMCONTROL_ENTRY(VMControl_VMToolsLastActive),
    { NULL, 0, NULL }
};

static vmcontrol_wrapper_api_t *vmcontrol_api = NULL;

vmcontrol_wrapper_api_t *vmcontrol_wrapper_api_get(void)
{
    return vmcontrol_api;
}

typedef void (*any_function_t)(void);

static int unsupported_function(void *obj, ...)
{
    return 0;
}

int vmcontrol_wrapper_api_init(const char *lib)
{
    int i;
    char *api;
    int dl_debug = getenv("VMCONTROL_DEBUG") != NULL;

    if (vmcontrol_api) {
        return 0;
    }

    if (!lib) { /* sanity check */
        if (dl_debug) {
            fprintf(stderr, "[vmcontrol_init] lib==NULL\n");
        }
        return DL_ENOENT;
    }

    vmcontrol_api = malloc(sizeof(*vmcontrol_api));
    api = (char *)vmcontrol_api;
    memset(vmcontrol_api, 0, sizeof(*vmcontrol_api));

    if (!(vmcontrol_api->handle = DL_LOAD(lib))) {
        return DL_ERR;
    }

    for (i=0; vmcontrol_entries[i].name; i++) {
        any_function_t *ptr =
            (any_function_t *)(api + (int)(long)vmcontrol_entries[i].offset);

        *ptr =
            (any_function_t)DL_SYM(vmcontrol_api->handle,
                                   vmcontrol_entries[i].name);

        if ((*ptr == NULL) && vmcontrol_entries[i].alias) {
            *ptr =
                (any_function_t)DL_SYM(vmcontrol_api->handle,
                                       vmcontrol_entries[i].alias);

            if (dl_debug) {
                fprintf(stderr, "[vmcontrol_init] alias %s -> %s\n",
                        vmcontrol_entries[i].name,
                        vmcontrol_entries[i].alias);
            }
        }

        if (!*ptr) {
            if (dl_debug) {
                fprintf(stderr, "[vmcontrol_init] %s -> UNDEFINED\n",
                        vmcontrol_entries[i].name);
            }
            *ptr = (any_function_t)unsupported_function;
        }
    }

    if ((void *)vmcontrol_api->xVMControl_VMInit == (void *)&unsupported_function) {
        if (dl_debug) {
            fprintf(stderr, "[vmcontrol_init] %s unuseable\n", lib);
        }

        vmcontrol_wrapper_api_shutdown();
        return DL_ENOENT;
    }

    return 0;
}

int vmcontrol_wrapper_api_shutdown(void)
{
    if (vmcontrol_api) {
        if (vmcontrol_api->handle) {
            DL_CLOSE(vmcontrol_api->handle);
        }

        free(vmcontrol_api);
        vmcontrol_api = NULL;
    }

    return 0;
}

#endif /* WIN32 || linux */
