#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>

/*
 * eam can provide per-process measurements, but at the moment requires
 * a pid.  something we cannot store in the database.  however, we could
 * store a service port in the database (e.g. 8009 for tomcat ajp) and 
 * figure out the pid given that.
 *
 * it is possible via /proc/net/tcp and /proc/$pid/fd/
 * to map a tcp port to one (or more if forked) process id(s).
 * caveats: expensive to scan /proc/$pid/fd
 *          must own process or be root to read /proc/$pid/fd
 * brutal to provide similar functionality on other platforms,
 * tho possible (see lsof).
 */

static char *sigar_skip_token(char *p)
{
    while (isspace(*p)) p++;
    while (*p && !isspace(*p)) p++;
    return p;
}

static char *sigar_skip_multiple_token(char *p, int count)
{
    int i;
    
    for (i = 0; i < count; i++) {
        p = sigar_skip_token(p);
    }

    return p;
}

static int get_process_ppid(char *pid, char *buf, int buflen)
{
    FILE *fp;
    char buffer[1024], *ptr, *ptr2;
    sprintf(buffer, "/proc/%s/stat", pid);

    if (!(fp = fopen(buffer, "r"))) {
        return errno;
    }

    ptr = fgets(buffer, sizeof(buffer), fp);
    fclose(fp);

    ptr = sigar_skip_multiple_token(ptr, 3);

    while (isspace(*ptr)) {
        ++ptr;
    }

    ptr2 = strchr(ptr, ' ');
    *ptr2 = '\0';

    strncpy(buf, ptr2, buflen);
    buf[buflen] = '\0';

    return 0;
}

typedef struct {
    int port;
    int inode;
    int uid;
    int pid;
    char name[1024];
} portmap_t;

static int portmap_lookup_inode(portmap_t *map) {
    FILE *fp;
    char buffer[8192], *ptr;
    int inode = 0;

    if ((fp = fopen("/proc/net/tcp", "r")) < 0) {
        return -1;
    }

    fgets(buffer, sizeof(buffer), fp); /* skip header */

    while ((ptr = fgets(buffer, sizeof(buffer), fp))) {
        ptr = sigar_skip_token(ptr);

        if ((ptr = strchr(ptr, ':'))) {
            unsigned long port = strtoul(++ptr, &ptr, 16);

            if (!(map->port == (port & 0xffff))) {
                continue;
            }
        }

        ptr = sigar_skip_multiple_token(ptr, 5);
        map->uid = strtoul(ptr, &ptr, 10);

        ptr = sigar_skip_token(ptr);

        inode = map->inode = strtoul(ptr, &ptr, 10);

        break;
    }

    fclose(fp);

    return inode ? 0 : ENOENT;
}

static int portmap_lookup_pid(portmap_t *map, char *pid) {
    DIR *dirp;
    struct dirent *dp;
    char fname[1024], dname[1024];

    map->pid = -1;

    sprintf(dname, "/proc/%s/fd", pid);

    if (!(dirp = opendir(dname))) {
        return errno;
    }

    while ((dp = readdir(dirp))) {
        struct stat statbuf;

        if (!isdigit(*dp->d_name)) {
            continue;
        }

        sprintf(fname, "%s/%s", dname, dp->d_name);
        //printf("check %s\n", fname);
        if (stat(fname, &statbuf)) {
            continue;
        }

        if (statbuf.st_ino == map->inode) {
            map->pid = atoi(pid);
            break;
        }
    }
    
    closedir(dirp);

    return map->pid == -1 ? ENOENT : 0;
}

static int portmap_lookup_pid_scan(portmap_t *map) {
    DIR *dirp;
    struct dirent *dp;

    char dname[1024];

    if (!(dirp = opendir("/proc"))) {
        return -1;
    }

    map->pid = -1;

    while ((dp = readdir(dirp))) {
        struct stat statbuf;

        if (!isdigit(*dp->d_name)) {
            continue;
        }

        sprintf(dname, "/proc/%s", dp->d_name);
        if (stat(dname, &statbuf)) {
            continue;
        }

        if (!(statbuf.st_uid == map->uid)) {
            continue;
        }

        if (portmap_lookup_pid(map, dp->d_name) == 0) {
#if 0
            char *pid = dp->d_name;
            char ppid[16];
            get_process_ppid(pid, ppid, sizeof(ppid));
#endif
            //break;
            printf("%s has inode %d\n", dp->d_name, map->inode);
        }
    }

    closedir(dirp);

    return map->pid == -1 ? ENOENT : 0;
}

int main(int argc, char **argv) {
    portmap_t map;

    if (argc != 2) {
        printf("usage: %s port\n", argv[0]);
    }

    map.port = atoi(argv[1]);

    if (portmap_lookup_inode(&map) == 0) {
        if (portmap_lookup_pid_scan(&map) == 0) {
            printf("inode=%d, pid=%d\n", map.inode, map.pid);
        }
    }

    return 1;
}
