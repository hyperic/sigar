#include <stdio.h>

#include "sigar.h"

int main(int argc, char *argv[]){
    sigar_t *sig;

    if(sigar_open(&sig) != SIGAR_OK){
        fprintf(stderr, "Error opening sigar context!\n");
        return -1;
    }

    sigar_close(sig);
    return 0;
}
