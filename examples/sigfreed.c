#include <stdio.h>
#include <string.h>

#include "sigar.h"

int main(int argc, char *argv[]){
    sigar_swap_t swapinfo;
    sigar_mem_t meminfo;
    sigar_t *sig;
    int err;

    if(sigar_open(&sig) != SIGAR_OK){
        fprintf(stderr, "Error opening sigar context!\n");
        return -1;
    }

    if((err = sigar_mem_get(sig, &meminfo)) != SIGAR_OK ||
       (err = sigar_swap_get(sig, &swapinfo)) != SIGAR_OK)
    {
        fprintf(stderr, "Failed to get memory info: %s\n",
                strerror(err));
        sigar_close(sig);
        return -1;
    }

    fprintf(stdout, "%18s %10s %10s %10s %10s %10s\n", 
            "total", "used", "free", "shared", "buffers", "cached");
    fprintf(stdout, "Mem:    %10ld %10ld %10ld %10ld %10ld %10ld\n",
            meminfo.total / 1024, 
            meminfo.used / 1024, 
            meminfo.free / 1024, 
            meminfo.shared / 1024,
            meminfo.buffer / 1024,
            meminfo.cached / 1024);
    fprintf(stdout, "-/+ buffers/cache: %10ld %10d\n",
            meminfo.actual_used / 1024,
            meminfo.actual_free / 1024);
    fprintf(stdout, "Swap:   %10ld %10ld %10ld\n",
            swapinfo.total / 1024,
            swapinfo.used / 1024,
            swapinfo.free / 1024);

    sigar_close(sig);
    return 0;
}
