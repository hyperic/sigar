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
int sigar_perfstat_cpu_total(perfstat_cpu_total_t *cpu_total)
{
    return perfstat_cpu_total(NULL, cpu_total, sizeof(*cpu_total), 1);
}

int sigar_perfstat_cpu(perfstat_id_t *id,
                       perfstat_cpu_t *cpu,
                       int num)
{
    return perfstat_cpu(id, cpu, sizeof(*cpu), num);
}

int sigar_perfstat_pagingspace(perfstat_id_t *id,
                               perfstat_pagingspace_t *pagingspace,
                               int num)
{
    return perfstat_pagingspace(id, pagingspace, sizeof(*pagingspace), num);
}

int sigar_perfstat_memory(perfstat_memory_total_t *memory)
{
    return perfstat_memory_total(NULL, memory, sizeof(*memory), 1);
}

int sigar_perfstat_disk(perfstat_id_t *id,
                        perfstat_disk_t *disk,
                        int num)
{
    return perfstat_disk(id, disk, sizeof(*disk), num);
}

int sigar_perfstat_diskadapter(perfstat_id_t *id,
                               perfstat_diskadapter_t *adapter,
                               int num)
{
    return perfstat_diskadapter(id, adapter, sizeof(*adapter), num);
}

int sigar_perfstat_diskpath(perfstat_id_t *id,
                            perfstat_diskpath_t *diskpath,
                            int num)
{
    return perfstat_diskpath(id, diskpath, sizeof(*diskpath), num);
}

int sigar_perfstat_netinterface(perfstat_id_t *id,
                                perfstat_netinterface_t *netinterface)

{
    return perfstat_netinterface(id, netinterface,
                                 sizeof(*netinterface), 1);
}

int sigar_perfstat_netbuffer(perfstat_id_t *id,
                             perfstat_netbuffer_t *netbuffer,
                             int num)
{
    return perfstat_netbuffer(id, netbuffer, sizeof(*netbuffer), num);
}

int sigar_perfstat_protocol(perfstat_id_t *id,
                            perfstat_protocol_t *proto,
                            int num)
{
    return perfstat_protocol(id, proto, sizeof(*proto), num);
}

/*
 * not a perfstat function, but from libpthreads.a
 * requires the same tricks to link on 4.3 and run on 5.2
 */ 
int sigar_thread_rusage(struct rusage *usage, int mode)
{
    return pthread_getrusage_np(pthread_self(), usage, mode);
}
