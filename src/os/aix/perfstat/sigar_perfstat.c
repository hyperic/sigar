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

/*
 * not a perfstat function, but from libpthreads.a
 * requires the same tricks to link on 4.3 and run on 5.2
 */ 
int sigar_thread_rusage(struct rusage *usage, int mode)
{
    return pthread_getrusage_np(pthread_self(), usage, mode);
}
