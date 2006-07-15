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

#define WIN32
#if defined(WIN32)
#define WINDOWS_LEAN_AND_MEAN
#define FILESEP '\\'
#define LIBPRE ""
#include "windows.h"
#else
#define FILESEP '/'
#define LIBPRE "lib"
#include <dlfcn.h>
#include <strings.h>
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "sigar.h"
#include "sigar_fileinfo.h"

#if defined(WIN32)
#  define LIBEXT "dll"
#elif defined(DARWIN)
#  define LIBEXT "dylib"
#else
#  define LIBEXT "so"
#endif

typedef struct {
    const char *name;
    void *func;
} sigar_callback_t;

struct {
    struct {
        const char *name;
        sigar_version_t * (*func)(void);
    } version;

    struct {
        const char *name;
        int (*func)(sigar_t **);
    } open;

    struct {
        const char *name;
        int (*func)(sigar_t *);
    } close;
    
    struct {
        const char *name;
        int (*func)(sigar_t *, sigar_pid_t, sigar_proc_exe_t *);
    } proc_exe;
    struct {
        const char *name;
        int (*func)(sigar_t *, sigar_pid_t, sigar_proc_args_t *);
    } proc_args;
    struct {
        const char *name;
        int (*func)(sigar_t *, sigar_pid_t, sigar_proc_env_t *);
    } proc_env;
    struct {
        const char *name;
        int (*func)(sigar_t *, sigar_pid_t, sigar_proc_modules_t *);
    } proc_modules;
    struct {
        const char *name;
        int (*func)(sigar_t *, sigar_pid_t, sigar_proc_fd_t *);
    } proc_fd;
    struct {
        const char *name;
        int (*func)(sigar_t *, int, unsigned long, sigar_pid_t *);
    } proc_port;

    struct {
        const char *name;
        int (*func)(sigar_t *, const char *dir, sigar_dir_stat_t *);
    } dir_stat;
    struct {
        const char *name;
        int (*func)(sigar_t *, const char *dir, sigar_dir_usage_t *);
    } dir_usage;
    sigar_callback_t end;
} sigar_callbacks = {
    { "sigar_version_get", NULL },
    { "sigar_open", NULL },
    { "sigar_close", NULL },
    { "sigar_proc_exe_get", NULL },
    { "sigar_proc_args_get", NULL },
    { "sigar_proc_env_get", NULL },
    { "sigar_proc_modules_get", NULL },
    { "sigar_proc_fd_get", NULL },
    { "sigar_proc_port_get", NULL },
    { "sigar_dir_stat_get", NULL },
    { "sigar_dir_usage_get", NULL },
    { NULL, NULL }
}; 

static int sigar_main(char *argv0)
{
    int status;
    sigar_t *sigar;
    char *errmsg;
    char *ptr;
    char sigarlib[8096], archlib[512];
    void *handle;
    sigar_callback_t *callbacks =
        (sigar_callback_t *)&sigar_callbacks;

    strcpy(sigarlib, argv0);
#ifdef WIN32
    if ((ptr = strstr(sigarlib, ".exe"))) {
        *ptr = '\0';
    }
#endif
    ptr = strrchr(sigarlib, FILESEP);
    if (ptr) {
        ++ptr;
    }
    else {
        ptr = sigarlib;
    }

    sprintf(archlib, LIBPRE "%s." LIBEXT, ptr);
    strcpy(ptr, archlib);

#if defined(__sun)
    dlopen("/usr/lib/libnsl.so", RTLD_NOW|RTLD_GLOBAL);
#endif

#if defined(WIN32)
    if (!(handle = LoadLibrary(sigarlib))) {
        errmsg = "XXX FormatMessage";
    }
#else
    if (!(handle = dlopen(sigarlib, RTLD_LAZY))) {
        errmsg = dlerror();
    }
#endif

    if (!handle) {
        fprintf(stderr, "Error opening '%s': %s\n",
                sigarlib, errmsg);
        exit(1);
    }

    while (callbacks->name) {
#if defined(WIN32)
        callbacks->func = GetProcAddress(handle, callbacks->name);
#else
        callbacks->func = dlsym(handle, callbacks->name);
#endif
        callbacks++;
    }

    if (isatty(fileno(stdin))) {
        sigar_version_t *version;

        if (!sigar_callbacks.version.func) {
            exit(1);
        }

        version = sigar_callbacks.version.func();

        printf("version=%s, build date=%s\n",
               version->version, version->build_date);
#if defined(WIN32)
        FreeLibrary(handle);
#else
        dlclose(handle);
#endif
        exit(0);
    }

    status = sigar_callbacks.open.func(&sigar);

    if (status != SIGAR_OK) {
        perror("sigar_open");
        return 1;
    }

    /* XXX pipe loop */

    sigar_callbacks.close.func(sigar);
#if defined(WIN32)
    FreeLibrary(handle);
#else
    dlclose(handle);
#endif
    return 0;
}

int main(int argc, char **argv)
{
    if (argc == 1) {
        return sigar_main(argv[0]);
    }
    else {
        return 1; /*XXX*/
    }
}
