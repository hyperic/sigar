#include <stdio.h>

#include "sigar.h"

int main(int argc, char **argv) {
    int status, i;
    sigar_t *sigar;
    sigar_proc_list_t proclist;

    sigar_open(&sigar);

    status = sigar_proc_list_get(sigar, &proclist);

    if (status != SIGAR_OK) {
        printf("proc_list error: %d (%s)\n",
               status, sigar_strerror(sigar, status));
        exit(1);
    }

    for (i=0; i<proclist.number; i++) {
        sigar_pid_t pid = proclist.data[i];
        sigar_proc_state_t pstate;
        sigar_proc_time_t ptime;

        status = sigar_proc_state_get(sigar, pid, &pstate);
        if (status != SIGAR_OK) {
#ifdef DEBUG
            printf("error: %d (%s) proc_state(%d)\n",
                   status, sigar_strerror(sigar, status), pid);
#endif
            continue;
        }

        status = sigar_proc_time_get(sigar, pid, &ptime);
        if (status != SIGAR_OK) {
#ifdef DEBUG
            printf("error: %d (%s) proc_time(%d)\n",
                   status, sigar_strerror(sigar, status), pid);
#endif
            continue;
        }

        printf("%d %s\n", (long)pid, pstate.name);
    }

    sigar_proc_list_destroy(sigar, &proclist);

    sigar_close(sigar);

    return 0;
}
