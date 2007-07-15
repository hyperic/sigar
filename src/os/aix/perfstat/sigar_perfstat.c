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

#include <libperfstat.h>
#include <pthread.h>

/*
 * ibm docs say:
 * "name Must be set to NULL.
 *  desired_number Must be set to 1."
 * so we just hardcode that in our wrapper.
 */
int sigar_perfstat_cpu_total(perfstat_cpu_total_t *cpu_total, size_t size)
{
    return perfstat_cpu_total(NULL, cpu_total, size, 1);
}

int sigar_perfstat_cpu(perfstat_id_t *id,
                       perfstat_cpu_t *cpu,
                       size_t size, int num)
{
    return perfstat_cpu(id, cpu, size, num);
}

int sigar_perfstat_pagingspace(perfstat_id_t *id,
                               perfstat_pagingspace_t *pagingspace,
                               size_t size,
                               int num)
{
    return perfstat_pagingspace(id, pagingspace, size, num);
}

int sigar_perfstat_memory_total(perfstat_id_t *id,
                                perfstat_memory_total_t *memory,
                                size_t size,
                                int num)
{
    return perfstat_memory_total(id, memory, size, num);
}

int sigar_perfstat_disk(perfstat_id_t *id,
                        perfstat_disk_t *disk,
                        size_t size,
                        int num)
{
    return perfstat_disk(id, disk, size, num);
}

int sigar_perfstat_diskadapter(perfstat_id_t *id,
                               perfstat_diskadapter_t *adapter,
                               size_t size,
                               int num)
{
    return perfstat_diskadapter(id, adapter, size, num);
}

int sigar_perfstat_diskpath(perfstat_id_t *id,
                            perfstat_diskpath_t *diskpath,
                            size_t size,
                            int num)
{
    return perfstat_diskpath(id, diskpath, size, num);
}

int sigar_perfstat_netinterface(perfstat_id_t *id,
                                perfstat_netinterface_t *netinterface)

{
    return perfstat_netinterface(id, netinterface,
                                 sizeof(*netinterface), 1);
}

int sigar_perfstat_netbuffer(perfstat_id_t *id,
                             perfstat_netbuffer_t *netbuffer,
                             size_t size,
                             int num)
{
    return perfstat_netbuffer(id, netbuffer, size, num);
}

int sigar_perfstat_protocol(perfstat_id_t *id,
                            perfstat_protocol_t *proto,
                            int size,
                            int num)
{
    return perfstat_protocol(id, proto, size, num);
}

/*
 * not a perfstat function, but from libpthreads.a
 * requires the same tricks to link on 4.3 and run on 5.2
 */ 
int sigar_thread_rusage(struct rusage *usage, int mode)
{
    return pthread_getrusage_np(pthread_self(), usage, mode);
}
