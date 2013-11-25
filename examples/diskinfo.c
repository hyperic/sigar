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
#include <unistd.h>
#include <assert.h>
#include <string.h>

#include "sigar.h"

int getNumShards(const char *dbpath) {
    sigar_t *sigar;

    sigar_file_system_list_t fslist;
    sigar_file_system_usage_t fsu;

    if (sigar_open(&sigar) == SIGAR_OK &&
        sigar_file_system_list_get(sigar, &fslist) == SIGAR_OK) {

        sigar_file_system_usage_t fsu;
        sigar_file_system_t fsi;
        int best_match = 0;
        char *mnt = NULL;
        int i,j;
        for (i = 0, j = 0; i < fslist.number; i++) {
            sigar_file_system_t fs = fslist.data[i];
            if (strstr(dbpath, fs.dir_name)) {
                size_t len = strlen(fs.dir_name);
                if (len > best_match) {
                    best_match = len;
                    sigar_file_system_usage_t fsusage;
                    if (sigar_file_system_usage_get(sigar, fs.dir_name,
                                                    &fsusage) == SIGAR_OK) {
                        fsu = fsusage;
                        fsi  = fs;
                    }
                }
            }
        }

        sigar_file_system_list_destroy(sigar, &fslist);
        sigar_close(sigar);

        printf("For dbpath %s best mount point is %s dev name is %s:\n",
                dbpath, fsi.dir_name, fsi.dev_name);
        printf("Write Time =" SIGAR_F_U64 " Read Time =" SIGAR_F_U64
               " Service time = %f Queue =%f"
                " FS Use = %f%% \n",
                fsu.disk.wtime, fsu.disk.rtime, fsu.disk.service_time,
                fsu.disk.queue, (fsu.use_percent*100));

        if (fsu.disk.wtime > fsu.disk.rtime + fsu.disk.rtime >> 1) {
            printf("Likely disk type: SSD\n");
            return 4;
        } else {
            printf("Likely disk type: Spindle Disk\n");
            return 2;
        }
    }

    return 4;
}

int main(int argc, char **argv) {
    getNumShards(argv[1]);

    return 0;
}
