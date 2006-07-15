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

#ifdef DARWIN
#include <mach/port.h>
#include <mach/host_info.h>
#else
#include <kvm.h>
#endif

#include <sys/sysctl.h>

enum {
    KOFFSET_CPUINFO,
    KOFFSET_VMMETER,
    KOFFSET_MAX
};

struct sigar_t {
    SIGAR_T_BASE;
    int pagesize;
    time_t last_getprocs;
    sigar_pid_t last_pid;
    struct kinfo_proc *pinfo;
#ifdef DARWIN
    mach_port_t mach_port;
#else
    kvm_t *kmem;
    /* offsets for seeking on kmem */
    unsigned long koffsets[KOFFSET_MAX];
    int proc_mounted;
#endif
};

#define SIGAR_EPERM_KMEM (SIGAR_OS_START_ERROR+EACCES)
#define SIGAR_EPROC_NOENT (SIGAR_OS_START_ERROR+2)

#endif /* SIGAR_OS_H */
