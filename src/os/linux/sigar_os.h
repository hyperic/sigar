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
    sigar_uint64_t minor_faults;
    sigar_uint64_t major_faults;
    sigar_uint64_t ppid;
    int tty;
    int priority;
    int nice;
    sigar_uint64_t start_time;
    sigar_uint64_t utime;
    sigar_uint64_t stime;
    char name[SIGAR_PROC_NAME_LEN];
    char state;
    int processor;
} linux_proc_stat_t;

typedef enum {
    IOSTAT_NONE,
    IOSTAT_PARTITIONS, /* 2.4 */
    IOSTAT_DISKSTATS, /* 2.6 */
    IOSTAT_SYS /* 2.6 */
} linux_iostat_e;

struct sigar_t {
    SIGAR_T_BASE;
    int pagesize;
    int ram;
    int proc_signal_offset;
    linux_proc_stat_t last_proc_stat;
    int lcpu;
    linux_iostat_e iostat;
    char *proc_net;
    /* Native POSIX Thread Library 2.6+ kernel */
    int has_nptl;
};

#define HAVE_STRERROR_R
#ifndef __USE_XOPEN2K
/* use gnu version of strerror_r */
#define HAVE_STRERROR_R_GLIBC
#endif
#define HAVE_READDIR_R
#define HAVE_GETPWNAM_R
#define HAVE_GETPWUID_R
#define HAVE_GETGRGID_R

#endif /* SIGAR_OS_H */
