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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <assert.h>

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

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

char *sigar_getword(char **line, char stop)
{
    char *pos = *line;
    int len;
    char *res;

    while ((*pos != stop) && *pos) {
        ++pos;
    }

    len = pos - *line;
    res = malloc(len + 1);
    memcpy(res, *line, len);
    res[len] = 0;

    if (stop) {
        while (*pos == stop) {
            ++pos;
        }
    }

    *line = pos;

    return res;
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
           (SSTRLEN(PROCP_FS_ROOT) + UITOA_BUFFER_SIZE + fname_len + 1));

    memcpy(ptr, PROCP_FS_ROOT, SSTRLEN(PROCP_FS_ROOT));
    ptr += SSTRLEN(PROCP_FS_ROOT);

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

int sigar_procfs_args_get(sigar_t *sigar, sigar_pid_t pid,
                          sigar_proc_args_t *procargs)
{
    char buffer[9086], *buf=NULL, *ptr;
    int fd, len, total=0;

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/cmdline");

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        if (errno == ENOENT) {
            return ESRCH;
        }
        return errno;
    }

    buffer[0] = '\0';

    /* XXX: possible to get rid of some mallocs here.
     * but, unlikely this will be called often so it
     * might not even matter much.
     */
    while ((len = read(fd, buffer, sizeof(buffer)-1)) > 0) {
        if (len == 0) {
            break;
        }
        buf = realloc(buf, total+len+1);
        memcpy(buf+total, buffer, len);
        total += len;
    }

    close(fd);

    /* e.g. /proc/2/cmdline */
    if (total == 0) {
        procargs->number = 0;
        return SIGAR_OK;
    }

    buf[total] = '\0';
    ptr = buf;

    while (total > 0) {
        int alen = strlen(ptr)+1;
        char *arg = malloc(alen);

        SIGAR_PROC_ARGS_GROW(procargs);
        memcpy(arg, ptr, alen);

        procargs->data[procargs->number++] = arg;

        total -= alen;
        if (total > 0) {
            ptr += alen;
        }
    }

    free(buf);

    return SIGAR_OK;
}

#endif /* WIN32 */

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

#define IS_CPU_R(p) \
   ((*p == '(') && (*(p+1) == 'R') && (*(p+2) == ')'))

typedef struct {
    char *name;  /* search */
    int len;
    char *rname; /* replace */
    int rlen;
} cpu_model_str_t;

/* to later replace 's' with 'r' */
#define CPU_MODEL_ENT_R(s, r) \
    { s, sizeof(s)-1, r, sizeof(r) }

#define CPU_MODEL_ENT(s) \
    CPU_MODEL_ENT_R(s, s)

/* after the vendor part of the string is removed,
 * looking for startsWith the entries below
 * to remove the crap after the model name, see
 * ../exp/intel_amd_cpu_models.txt
 */
static const cpu_model_str_t cpu_models[] = {
    /* intel */
    CPU_MODEL_ENT("Xeon"),
    CPU_MODEL_ENT_R("XEON", "Xeon"),
    CPU_MODEL_ENT("Pentium III"),
    CPU_MODEL_ENT("Pentium II"),
    CPU_MODEL_ENT_R("Pentium(R) III", "Pentium III"),
    CPU_MODEL_ENT_R("Pentium(R) 4", "Pentium 4"),
    CPU_MODEL_ENT_R("Pentium(R) M", "Pentium M"),
    CPU_MODEL_ENT("Pentium Pro"),
    CPU_MODEL_ENT("Celeron"),

    /* amd */
    CPU_MODEL_ENT("Opteron"),
    CPU_MODEL_ENT("Athlon"),
    CPU_MODEL_ENT("Duron"),
    CPU_MODEL_ENT_R("K6(tm)-III", "K6 III"),
    CPU_MODEL_ENT_R("K6(tm) 3D+", "K6 3D+"),
    { NULL }
};

/* common to win32 and linux */
void sigar_cpu_model_adjust(sigar_t *sigar, sigar_cpu_info_t *info)
{
    int len, i;
    char model[128], *ptr=model, *end;

    memcpy(model, info->model, sizeof(model));

    /* trim leading and trailing spaces */
    len = strlen(model);
    end = &model[len-1];
    while (*ptr == ' ') ++ptr;
    while (*end == ' ') *end-- = '\0';

    /* remove vendor from model name */
    len = strlen(info->vendor);
    if (strnEQ(ptr, info->vendor, len)) {
        ptr += len;
        if (IS_CPU_R(ptr)) {
            ptr += 3; /* remove (R) */
        }
        while (*ptr == ' ') ++ptr;
    }

    if (*ptr == '-') {
        ++ptr; /* e.g. was AMD-K6... */
    }

    for (i=0; cpu_models[i].name; i++) {
        const cpu_model_str_t *cpu_model = &cpu_models[i];

        if (strnEQ(ptr, cpu_model->name, cpu_model->len)) {
            memcpy(info->model, cpu_model->rname, cpu_model->rlen);
            return;
        }
    }

    strcpy(info->model, ptr);
}

/* attempt to derive MHz from model name
 * currently works for certain intel strings
 * see exp/intel_amd_cpu_models.txt
 */ 
int sigar_cpu_mhz_from_model(char *model)
{
    int mhz = SIGAR_FIELD_NOTIMPL;
    char *ptr = model;

    while (*ptr && (ptr = strchr(ptr, ' '))) {
        while(*ptr && !sigar_isdigit(*ptr)) {
            ptr++;
        }
        mhz = sigar_strtoul(ptr);

        if (*ptr == '.') {
            /* e.g. "2.40GHz" */
            ++ptr;
            mhz *= 100;
            mhz += sigar_strtoul(ptr);
            break;
        }
        else if (strnEQ(ptr, "GHz", 3) ||
                 strnEQ(ptr, "MHz", 3))
        {
            /* e.g. "1500MHz" */
            break;
        }
        else {
            mhz = SIGAR_FIELD_NOTIMPL;
        }
    }

    if (mhz != SIGAR_FIELD_NOTIMPL) {
        if (strnEQ(ptr, "GHz", 3)) {
            mhz *= 10;
        }
    }

    return mhz;
}

#if !defined(WIN32) && !defined(NETWARE)
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>
#ifdef SIGAR_HPUX
#include <nfs/nfs.h>
#endif
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__sun) || defined(DARWIN)
#include <arpa/inet.h>
#endif
#if defined(__sun) || defined(SIGAR_HPUX)
#include <rpc/clnt_soc.h>
#endif
#if defined(_AIX) || defined(SIGAR_HPUX) || defined(__OpenBSD__)
#include <sys/socket.h>
#endif

static enum clnt_stat get_sockaddr(struct sockaddr_in *addr, char *host)
{
    register struct hostent *hp;
    sigar_hostent_t data;

    memset(addr, 0, sizeof(struct sockaddr_in));
    addr->sin_family = AF_INET;

    if ((addr->sin_addr.s_addr = inet_addr(host)) == -1) {
        if (!(hp = sigar_gethostbyname(host, &data))) {
            return RPC_UNKNOWNHOST;
        }
        memcpy(&addr->sin_addr, hp->h_addr, hp->h_length);
    }

    return RPC_SUCCESS;
}

char *sigar_rpc_strerror(int err)
{
    return (char *)clnt_sperrno(err);
}

SIGAR_DECLARE(int) sigar_rpc_ping(char *host,
                                  int protocol,
                                  unsigned long program,
                                  unsigned long version)
{
    CLIENT *client;
    struct sockaddr_in addr;
    int sock;
    struct timeval timeout;
    unsigned short port = 0;
    enum clnt_stat rpc_stat; 

    rpc_stat = get_sockaddr(&addr, host);
    if (rpc_stat != RPC_SUCCESS) {
        return rpc_stat;
    }

    timeout.tv_sec = 2;
    timeout.tv_usec = 0;
    addr.sin_port = htons(port);
    sock = RPC_ANYSOCK;
    
    if (protocol == SIGAR_NETCONN_UDP) {
        client =
            clntudp_create(&addr, program, version,
                           timeout, &sock);
    }
    else if (protocol == SIGAR_NETCONN_TCP) {
        client =
            clnttcp_create(&addr, program, version,
                           &sock, 0, 0);
    }
    else {
        return RPC_UNKNOWNPROTO;
    }

    if (!client) {
        return rpc_createerr.cf_stat;
    }

    timeout.tv_sec = 10;
    timeout.tv_usec = 0;
    rpc_stat = clnt_call(client, NULLPROC, (xdrproc_t)xdr_void, NULL,
                         (xdrproc_t)xdr_void, NULL, timeout);

    if (rpc_stat != RPC_SUCCESS) {
        return rpc_stat;
    }

    clnt_destroy(client);

    return RPC_SUCCESS;
}
#endif

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

#ifdef WIN32
#define vsnprintf _vsnprintf
#endif

#ifdef WIN32
#   define rindex strrchr
#endif

static int proc_module_get_self(void *data, char *name, int len)
{
    sigar_t *sigar = (sigar_t *)data;
    char *ptr = rindex(name, '/');

    if (!ptr) {
        return SIGAR_OK;
    }

    if (strnEQ(ptr+1, "libsigar-", 9)) {
        int offset = ptr - name;

        sigar->self_path = sigar_strdup(name);
        *(sigar->self_path + offset) = '\0'; /* chop libsigar-*.so */

        if (SIGAR_LOG_IS_DEBUG(sigar)) {
            sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                             "detected sigar-lib='%s'",
                             sigar->self_path);
        }

        return !SIGAR_OK; /* break loop */
    }

    return SIGAR_OK;
}

char *sigar_get_self_path(sigar_t *sigar)
{
    if (!sigar->self_path) {
        sigar_proc_modules_t procmods;
        char *self_path = getenv("SIGAR_PATH");

        if (self_path) {
            sigar->self_path = sigar_strdup(self_path);
            return sigar->self_path;
        }

        procmods.module_getter = proc_module_get_self;
        procmods.data = sigar;

        sigar_proc_modules_get(sigar,
                               sigar_pid_get(sigar),
                               &procmods);

        if (!sigar->self_path) {
            /* dont try again */
            sigar->self_path = sigar_strdup(".");
        }
    }

    return sigar->self_path;
}

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
