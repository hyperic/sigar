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
        if (buf) {
            buf = realloc(buf, total+len);
        }
        else {
            buf = malloc(len+1);
        }
        memcpy(buf+total, buffer, len);
        total += len;
    }

    close(fd);

    sigar_proc_args_create(procargs);

    //e.g. /proc/2/cmdline
    if (total == 0) {
        procargs->number = 0;
        return SIGAR_OK;
    }

    buf[total] = '\0';
    ptr = buf;

    while (*ptr) {
        int alen = strlen(ptr)+1;
        char *arg = malloc(alen);

        SIGAR_PROC_ARGS_GROW(procargs);
        memcpy(arg, ptr, alen);

        procargs->data[procargs->number++] = arg;
            
        ptr += alen;
    }

    free(buf);

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
    CPU_MODEL_ENT("Pentium II"),
    CPU_MODEL_ENT("Pentium III"),
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
    while (*end == ' ') *--end = '\0';

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

#ifndef WIN32
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>
#include <rpcsvc/nfs_prot.h>
#ifdef __FreeBSD__
#include <arpa/inet.h>
#endif

static int get_sockaddr(struct sockaddr_in *addr, char *host)
{
    register struct hostent *hp;

    memset(addr, 0, sizeof(struct sockaddr_in));
    addr->sin_family = AF_INET;

    if ((addr->sin_addr.s_addr = inet_addr(host)) == -1) {
        if (!(hp = gethostbyname(host))) {
            return -1;
        }
        memcpy(&addr->sin_addr, hp->h_addr, hp->h_length);
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_nfs_ping(char *host)
{
    CLIENT *client;
    struct sockaddr_in addr;
    int sock, retval=SIGAR_OK;
    struct timeval timeout, interval;
    unsigned short port = 0;
    enum clnt_stat rpc_stat; 

    if (get_sockaddr(&addr, host) != SIGAR_OK) {
        return -1;
    }

    interval.tv_sec = 2;
    interval.tv_usec = 0;
    addr.sin_port = htons(port);
    sock = RPC_ANYSOCK;
    client = clntudp_create(&addr, NFS_PROGRAM, NFS_VERSION,
                            interval, &sock);
    if (!client) {
        return -1;
    }

    timeout.tv_sec = 10;
    timeout.tv_usec = 0;
    rpc_stat = clnt_call(client, NULLPROC, (xdrproc_t)xdr_void, NULL,
                         (xdrproc_t)xdr_void, NULL, timeout);

    if (rpc_stat != RPC_SUCCESS) {
        retval = -1;
    }

    close(sock);
    clnt_destroy(client);

    return retval;
}
#endif

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
