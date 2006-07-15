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

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

#define my_pread(fd, ptr, type, offset) \
    (pread(fd, ptr, sizeof(type), offset) == sizeof(type))

int sigar_proc_psinfo_get(sigar_t *sigar, sigar_pid_t pid)
{
    int fd, retval = SIGAR_OK;
    char buffer[BUFSIZ];
    time_t timenow = time(NULL);

    if (sigar->pinfo == NULL) {
        sigar->pinfo = malloc(sizeof(*sigar->pinfo));
    }

    if (sigar->last_pid == pid) {
        if ((timenow - sigar->last_getprocs) < SIGAR_LAST_PROC_EXPIRE) {
            return SIGAR_OK;
        }
    }

    sigar->last_pid = pid;
    sigar->last_getprocs = timenow;

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/psinfo");

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        return ESRCH;
    }

    if (!my_pread(fd, sigar->pinfo, psinfo_t, 0)) {
        retval = errno;
    }

    close(fd);

    return retval;
}

int sigar_proc_usage_get(sigar_t *sigar, prusage_t *prusage, sigar_pid_t pid)
{
    int fd, retval = SIGAR_OK;
    char buffer[BUFSIZ];

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/usage");

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        return ESRCH;
    }

    if (!my_pread(fd, prusage, prusage_t, 0)) {
        retval = errno;
    }

    close(fd);

    return retval;
}

int sigar_proc_status_get(sigar_t *sigar, pstatus_t *pstatus, sigar_pid_t pid)
{
    int fd, retval = SIGAR_OK;
    char buffer[BUFSIZ];

    (void)SIGAR_PROC_FILENAME(buffer, pid, "/status");

    if ((fd = open(buffer, O_RDONLY)) < 0) {
        return ESRCH;
    }

    if (!my_pread(fd, pstatus, pstatus_t, 0)) {
        retval = errno;
    }

    close(fd);

    return retval;
}
