#include <libperfstat.h>

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
