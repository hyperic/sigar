#include <sigar.h>
#include <stdio.h>
/*
gcc -g -o sigar_proc_args -lsocket \
-L ../bindings/java/sigar-bin/lib -lsigar-sparc-sun-solaris-2.x \
-I ../include sigar_proc_args.c
*/ 
int main(int argc, char **argv)
{
    sigar_t *sigar;
    sigar_proc_list_t procs;
    sigar_proc_args_t args;
    int n;

    if (sigar_open(&sigar) != SIGAR_OK) {
        fprintf(stderr, "Error opening sigar!\n");
        return -1;
    }

    sigar_proc_list_get(sigar, &procs);

    for (n=0; n<procs.number; n++) {
        pid_t pid = procs.data[n];
        int status = 
            sigar_proc_args_get(sigar, pid, &args);

        printf("-------------------------------\n");

        if (status == SIGAR_OK) {
            int i;

            printf("pid=%d\n", pid);

            for (i=0; i<args.number; i++) {
                printf("%d='%s'\n", i, args.data[i]);
            }

            sigar_proc_args_destroy(sigar, &args);
        }
        else {
            printf("pid=%d error=%d...%s\n",
                   pid, status, sigar_strerror(sigar, status));
        }
    }

    sigar_proc_list_destroy(sigar, &procs);
    sigar_close(sigar);

    return 0;
}
