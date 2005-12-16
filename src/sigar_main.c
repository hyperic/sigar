#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <unistd.h>
#include "sigar.h"
#include "sigar_fileinfo.h"

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
        int (*func)(sigar_t *, pid_t, sigar_proc_exe_t *);
    } proc_exe;
    struct {
        const char *name;
        int (*func)(sigar_t *, pid_t, sigar_proc_args_t *);
    } proc_args;
    struct {
        const char *name;
        int (*func)(sigar_t *, pid_t, sigar_proc_env_t *);
    } proc_env;
    struct {
        const char *name;
        int (*func)(sigar_t *, pid_t, sigar_proc_modules_t *);
    } proc_modules;
    struct {
        const char *name;
        int (*func)(sigar_t *, pid_t, sigar_proc_fd_t *);
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
    char *ptr;
    char sigarlib[8096], archlib[512];
    void *handle;
    sigar_callback_t *callbacks =
        (sigar_callback_t *)&sigar_callbacks;

    strcpy(sigarlib, argv0);
    ptr = rindex(sigarlib, '/');
    if (ptr) {
        ++ptr;
        sprintf(archlib, "lib%s.so", ptr);
        strcpy(ptr, archlib);
    }

#if defined(__sun)
    dlopen("/usr/lib/libnsl.so", RTLD_NOW|RTLD_GLOBAL);
#endif

    if (!(handle = dlopen(sigarlib, RTLD_LAZY))) {
        fprintf(stderr, "Error opening '%s': %s\n",
                sigarlib, dlerror());
        exit(1);
    }

    while (callbacks->name) {
        callbacks->func = dlsym(handle, callbacks->name);
        callbacks++;
    }

    if (isatty(fileno(stdin))) {
        sigar_version_t *version =
            sigar_callbacks.version.func();

        printf("version=%s, build date=%s\n",
               version->version, version->build_date);
        dlclose(handle);
        exit(0);
    }

    status = sigar_callbacks.open.func(&sigar);

    if (status != SIGAR_OK) {
        perror("sigar_open");
        return 1;
    }

    /* XXX pipe loop */

    sigar_callbacks.close.func(sigar);
    dlclose(handle);

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
