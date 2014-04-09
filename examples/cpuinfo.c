/*
 * Copyright (c) 2008 Hyperic, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <stdio.h>
#include <stdlib.h>

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

    printf("Number of CPUs: %d\n", (unsigned)cpulist.number);

    for (i=0; i<cpulist.number; i++) {
        sigar_cpu_t cpu = cpulist.data[i];
        printf("CPU %d: User=" SIGAR_F_U64 " Sys=" SIGAR_F_U64
               " Nice=" SIGAR_F_U64 " Idle=" SIGAR_F_U64 " Total="
               SIGAR_F_U64 "\n", i, cpu.user, cpu.sys, cpu.nice, cpu.idle,
               cpu.total);
        /*...*/
    }

    sigar_cpu_list_destroy(sigar, &cpulist);

	sigar_processor_queue_t proc_queue;
	status = sigar_processor_queue_get(sigar, &proc_queue);
    if (status != SIGAR_OK) {
        printf("processor_queue error: %d (%s)\n",
               status, sigar_strerror(sigar, status));
        exit(1);
    }
	printf("Processor Queue Length: %u\n", proc_queue.processor_queue);

    sigar_close(sigar);

    return 0;
}
