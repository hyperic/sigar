#include <libperfstat.h>

int sigar_perfstat_cpu_total(perfstat_id_t *id,
                             perfstat_cpu_total_t *cpu_total,
                             int size, int num)
{
    return perfstat_cpu_total(id, cpu_total, size, num);
}

int sigar_perfstat_cpu(perfstat_id_t *id,
                       perfstat_cpu_t *cpu,
                       int size, int num)
{
    return perfstat_cpu(id, cpu, size, num);
}

