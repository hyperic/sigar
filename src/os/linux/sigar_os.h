#ifndef SIGAR_OS_H
#define SIGAR_OS_H

#include <assert.h>

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <time.h>

#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>

typedef struct {
    sigar_pid_t pid;
    time_t mtime;
    sigar_uint64_t vsize;
    sigar_uint64_t rss;
    sigar_uint64_t ppid;
    int tty;
    int priority;
    int nice;
    sigar_uint64_t start_time;
    sigar_uint64_t utime;
    sigar_uint64_t stime;
    char name[SIGAR_PROC_NAME_LEN];
    char state;
} linux_proc_stat_t;

struct sigar_t {
    SIGAR_T_BASE;
    int ram;
    int proc_signal_offset;
    linux_proc_stat_t last_proc_stat;
    int ht_enabled;
    int lcpu;
};

#define HAVE_STRERROR_R
#define HAVE_STRERROR_R_GLIBC
#define HAVE_READDIR_R
#define HAVE_GETPWNAM_R
#define HAVE_GETPWUID_R
#define HAVE_GETGRGID_R

#endif /* SIGAR_OS_H */
