#include <stdio.h>

#include "sigar.h"

int main(int argc, char **argv) {
    int status, i;
    sigar_t *sigar;
    sigar_cpu_list_t cpulist;

    sigar_open(&sigar);

    status = sigar_cpu_list_get(sigar, &cpulist);

    if (status != SIGAR_OK) {
        printf("cpu_list error: %d (%s)\n",
               status, sigar_strerror(sigar, status));
        exit(1);
    }

    for (i=0; i<cpulist.number; i++) {
        sigar_cpu_t cpu = cpulist.data[i];
        /*...*/
    }

    sigar_cpu_list_destroy(sigar, &cpulist);

    sigar_close(sigar);

    return 0;
}
