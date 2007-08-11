#include <stdio.h>
#include "sigar.h"
#include "sigar_ptql.h"

int ptql_test(sigar_t *sigar, char *ptql) {
    int status, i;
    sigar_proc_list_t proclist;
    sigar_ptql_query_t *query;

    status =
        sigar_proc_list_get(sigar, &proclist);

    if (status != SIGAR_OK) {
        return status;
    }

    status =
        sigar_ptql_query_create(&query, ptql);

    if (status != SIGAR_OK) {
        return status;
    }

    for (i=0; i<proclist.number; i++) {
        sigar_pid_t pid = proclist.data[i];
        if (sigar_ptql_query_match(sigar, query, pid) == SIGAR_OK) {
            sigar_proc_state_t state;
            sigar_proc_state_get(sigar, pid, &state);
            printf("name=%s, pid=%d\n", state.name, (int)pid);
        }
    }

    sigar_proc_list_destroy(sigar, &proclist);

    sigar_ptql_query_destroy(query);

    return 0;
}

int main(int argc, char **argv)
{
    int i;
    sigar_t *sigar;

    if (sigar_open(&sigar) != SIGAR_OK) {
        return -1;
    }

    for (i=0; i<argc; i++) {
        ptql_test(sigar, argv[i]);
    }

    sigar_close(sigar);

    return 0;
}
