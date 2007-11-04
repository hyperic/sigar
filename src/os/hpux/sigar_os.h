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

#if defined(__ia64) && !defined(__ia64__)
#define __ia64__
#endif

#ifdef __ia64__
#ifndef _LP64
#define _LP64
#endif
#endif

#define _PSTAT64

#include <sys/pstat.h>
#include <sys/mib.h>
#include <stdlib.h>
#include <fcntl.h>

struct sigar_t {
    SIGAR_T_BASE;
    struct pst_static pstatic;
    time_t last_getprocs;
    sigar_pid_t last_pid;
    struct pst_status *pinfo;

    int mib;
};

int hpux_get_mib_ifentry(int ppa, mib_ifEntry *mib);

#endif /* SIGAR_OS_H */
