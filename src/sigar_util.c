#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <assert.h>

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"
#include "sigar_util.h"

#ifndef WIN32

#include <dirent.h>

SIGAR_INLINE char *sigar_uitoa(char *buf, unsigned int n, int *len)
{
    char *start = buf + UITOA_BUFFER_SIZE - 1;

    *start = 0;

    do {
	*--start = '0' + (n % 10);
        ++*len;
	n /= 10;
    } while (n);

    return start;
}

SIGAR_INLINE char *sigar_skip_line(char *buffer, int buflen)
{
    char *ptr = buflen ?
        (char *)memchr(buffer, '\n', buflen) : /* bleh */
        strchr(buffer, '\n');
    return ++ptr;
}

SIGAR_INLINE char *sigar_skip_token(char *p)
{
    while (sigar_isspace(*p)) p++;
    while (*p && !sigar_isspace(*p)) p++;
    return p;
}

SIGAR_INLINE char *sigar_skip_multiple_token(char *p, int count)
{
    int i;
    
    for (i = 0; i < count; i++) {
        p = sigar_skip_token(p);
    }

    return p;
}

int sigar_file2str(const char *fname, char *buffer, int buflen)
{
    int len;
    int fd = open(fname, O_RDONLY);

    if (fd < 0) {
        return ENOENT;
    }

    len = read(fd, buffer, buflen);
    buffer[len] = '\0';
    close(fd);

    return SIGAR_OK;
}

/* avoiding sprintf */

char *sigar_proc_filename(char *buffer, int buflen,
                          sigar_pid_t bigpid,
                          const char *fname, int fname_len)
{
    int len = 0;
    char *ptr = buffer;
    unsigned int pid = (unsigned int)bigpid; /* XXX -- This isn't correct */
    char pid_buf[UITOA_BUFFER_SIZE];
    char *pid_str = sigar_uitoa(pid_buf, pid, &len);

    assert((unsigned int)buflen >=
           (SSTRLEN(PROC_FS_ROOT) + UITOA_BUFFER_SIZE + fname_len + 1));

    memcpy(ptr, PROC_FS_ROOT, SSTRLEN(PROC_FS_ROOT));
    ptr += SSTRLEN(PROC_FS_ROOT);

    memcpy(ptr, pid_str, len);
    ptr += len;

    memcpy(ptr, fname, fname_len);
    ptr += fname_len;
    *ptr = '\0';

    return buffer;
}

int sigar_proc_file2str(char *buffer, int buflen,
                        sigar_pid_t pid,
                        const char *fname,
                        int fname_len)
{
    int retval;

    buffer = sigar_proc_filename(buffer, buflen, pid,
                                 fname, fname_len);

    retval = sigar_file2str(buffer, buffer, buflen);

    if (retval != SIGAR_OK) {
        switch (retval) {
          case ENOENT:
            retval = ESRCH; /* no such process */
          default:
            break;
        }
    }

    return retval;
}

int sigar_proc_list_procfs_get(sigar_t *sigar,
                               sigar_proc_list_t *proclist)
{
    DIR *dirp = opendir("/proc");
    struct dirent *ent;
#ifdef HAVE_READDIR_R
    struct dirent dbuf;
#endif

    if (!dirp) {
        return errno;
    }

    sigar_proc_list_create(proclist);

#ifdef HAVE_READDIR_R
    while (readdir_r(dirp, &dbuf, &ent) == 0) {
        if (ent == NULL) {
            break;
        }
#else
    while ((ent = readdir(dirp))) {
#endif
        if (!sigar_isdigit(*ent->d_name)) {
            continue;
        }

        /* XXX: more sanity checking */

        SIGAR_PROC_LIST_GROW(proclist);

        proclist->data[proclist->number++] =
            strtoul(ent->d_name, NULL, 10);
    }

    closedir(dirp);

    return SIGAR_OK;
}

int sigar_proc_fd_count(sigar_t *sigar, sigar_pid_t pid,
                        sigar_uint64_t *total)
{
    DIR *dirp;
    struct dirent *ent;
#ifdef HAVE_READDIR_R
    struct dirent dbuf;
#endif
    char name[BUFSIZ];

    (void)SIGAR_PROC_FILENAME(name, pid, "/fd");

    *total = 0;

    if (!(dirp = opendir(name))) {
        return errno;
    }

#ifdef HAVE_READDIR_R
    while (readdir_r(dirp, &dbuf, &ent) == 0) {
        if (ent == NULL) {
            break;
        }
#else
    while ((ent = readdir(dirp))) {
#endif
        if (!sigar_isdigit(*ent->d_name)) {
            continue;
        }

        (*total)++;
    }

    closedir(dirp);

    return SIGAR_OK;
}

#endif /* WIN32 */

/* os impls should use an optimized version of this */
int sigar_proc_count(sigar_t *sigar, sigar_uint64_t *total)
{
    int status;
    sigar_proc_list_t proclist;

    *total = 0;

    if ((status = sigar_proc_list_get(sigar, &proclist)) != SIGAR_OK) {
        return status;
    }

    *total = proclist.number;

    sigar_proc_list_destroy(sigar, &proclist);

    return SIGAR_OK;
}

int sigar_mem_calc_ram(sigar_t *sigar, sigar_mem_t *mem)
{
    sigar_uint64_t lram = (mem->total / (1024 * 1024));
    int ram = (int)lram; /* must cast after division */
    int remainder = ram % 8;

    if (remainder > 0) {
        ram += (8 - remainder);
    }

    mem->ram = ram;

    return ram;
}

double sigar_file_system_usage_calc_used(sigar_t *sigar,
                                         sigar_file_system_usage_t *fsusage)
{
    /* 
     * win32 will not convert __uint64 to double.
     * convert to KB then do unsigned long -> double.
     */
    sigar_uint64_t b_used = (fsusage->total - fsusage->free) / 1024;
    sigar_uint64_t b_avail = fsusage->avail / 1024;
    unsigned long utotal = b_used + b_avail;
    unsigned long used = b_used;

    if (utotal != 0) {
        unsigned long u100 = used * 100;
        double pct = u100 / utotal +
            ((u100 % utotal != 0) ? 1 : 0);
        return pct / 100;
    }

    return 0;
}

#ifdef WIN32
#define vsnprintf _vsnprintf
#endif

SIGAR_DECLARE(void) sigar_log_printf(sigar_t *sigar, int level,
                                     const char *format, ...)
{
    va_list args;
    char buffer[8192];

    if (level > sigar->log_level) {
        return;
    }

    if (!sigar->log_impl) {
        return;
    }

    va_start(args, format);
    vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);

    sigar->log_impl(sigar, sigar->log_data, level, buffer);
}

SIGAR_DECLARE(void) sigar_log(sigar_t *sigar, int level, char *message)
{
    if (level > sigar->log_level) {
        return;
    }

    if (!sigar->log_impl) {
        return;
    }

    sigar->log_impl(sigar, sigar->log_data, level, message);
}

SIGAR_DECLARE(void) sigar_log_impl_set(sigar_t *sigar, void *data,
                                       sigar_log_impl_t impl)
{
    sigar->log_data = data;
    sigar->log_impl = impl;
}

SIGAR_DECLARE(int) sigar_log_level_get(sigar_t *sigar)
{
    return sigar->log_level;
}

static const char *log_levels[] = {
    "FATAL",
    "ERROR",
    "WARN",
    "INFO",
    "DEBUG",
    "TRACE"
};

SIGAR_DECLARE(const char *) sigar_log_level_string_get(sigar_t *sigar)
{
    return log_levels[sigar->log_level];
}

SIGAR_DECLARE(void) sigar_log_level_set(sigar_t *sigar, int level)
{
    sigar->log_level = level;
}

SIGAR_DECLARE(void) sigar_log_impl_file(sigar_t *sigar, void *data,
                                        int level, char *message)
{
    FILE *fp = (FILE*)data;
    fprintf(fp, "[%s] %s\n", log_levels[level], message);
}
