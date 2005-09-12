#include <dirent.h>
#include <ctype.h>
#include <assert.h>
#include <malloc.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/processor.h>
#include <sys/sysinfo.h>
#include <sys/param.h>

#include <kstat.h>
#include <procfs.h>

#define PROC_ERRNO ((errno == ENOENT) ? ESRCH : errno)

#define my_pread(fd, ptr, type, offset) \
    (pread(fd, ptr, sizeof(type), offset) == sizeof(type))

static int proc_psinfo_get(psinfo_t *psinfo, pid_t pid)
{
    int fd, retval = 0;
    char buffer[BUFSIZ];

    sprintf(buffer, "/proc/%d/psinfo", pid);

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        return ESRCH;
    }

    if (!my_pread(fd, psinfo, psinfo_t, 0)) {
        retval = errno;
    }

    close(fd);

    return retval;
}

int main(int argc, char **argv)
{
    DIR *dirp = opendir("/proc");
    struct dirent *ent;
    char *models[] = {
        "unknown", "32bit", "64bit"
    };

    while ((ent = readdir(dirp))) {
        pid_t pid;
        psinfo_t psinfo;
        int retval;
        char buffer[BUFSIZ];
        char *argvb[56];
        char **argvp = argvb;

        int n, fd;
        size_t nread = 0;
        unsigned int argv_size;

        if (!isdigit(*ent->d_name)) {
            continue;
        }
        psinfo.pr_dmodel = 0;
        pid = strtoul(ent->d_name, NULL, 10);
        retval = proc_psinfo_get(&psinfo, pid);
        printf("---------------------------------\n");
        printf("pid=%d, status=%s, model=%s\n",
               pid, retval ? strerror(retval) : "OK",
               models[psinfo.pr_dmodel]);

        argv_size = sizeof(*argvp) * psinfo.pr_argc;
        sprintf(buffer, "/proc/%d/as", pid);
        printf("argc=%d, argv_size=%d\n",
               psinfo.pr_argc, argv_size);

        if ((fd = open(buffer, O_RDONLY)) < 0) {
            printf("open(%s) == %s\n",
                   buffer, strerror(PROC_ERRNO));
            if (argvp != argvb) {
                free(argvp);
            }
            continue;
        }

        if (argv_size > sizeof(argvb)) {
            argvp = malloc(argv_size);
        }

        if ((nread = pread(fd, argvp, argv_size, psinfo.pr_argv)) <= 0) {
            close(fd);
            printf("   pread(%d, 0x%lx, %d, 0x%lx) == %d (%s)\n",
                   fd, (unsigned long)argvp, argv_size,
                   (unsigned long)psinfo.pr_argv,
                   nread, strerror(errno));
            continue;
        }

        for (n = 0; n < psinfo.pr_argc; n++) {
            int alen;
            char *arg;

            if ((nread = pread(fd, buffer, sizeof(buffer), (off_t)argvp[n])) <= 0) {
                close(fd);
                printf("   %-2d) pread(%d, 0x%lx, %d, 0x%lx) == %d (%s)\n",
                       n, fd, (unsigned long)&buffer[0], sizeof(buffer),
                       (unsigned long)argvp[n],
                       nread, strerror(errno));
                continue;
            }

            printf("   %-2d) nread=%-4d, ", n, nread);
            fflush(stdout);
            alen = strlen(buffer)+1;
            printf(" alen=%-4d ", alen);
            fflush(stdout);
            arg = malloc(alen);
            memcpy(arg, buffer, alen);
            printf(" {%s}\n", arg);
            fflush(stdout);
        }

        if (argvp != argvb) {
            free(argvp);
        }

        close(fd);
    }

    closedir(dirp);

    return 0;
}

