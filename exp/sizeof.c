/*
#!perl -l
use Config;
my $prg = "$0.out";
my $cmd = "$Config{cc} -Wall -D$^O -o $prg $0; ./$prg; rm $prg";
print $cmd; system $cmd;
__END__
 */

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    const char *name;
    unsigned int size;
} sizeof_info_t;

#define SIZEOF_INFO(v) \
    {#v, sizeof(v)}

#ifdef solaris

#include <sys/kstat.h>
#include <sys/sysinfo.h>
#include <procfs.h>
#include <sys/proc.h>
#include <sys/user.h>

static sizeof_info_t sizeof_info[] = {
    /* kstat */
    SIZEOF_INFO(kstat_named_t),
    SIZEOF_INFO(vminfo_t),
    SIZEOF_INFO(cpu_stat_t),
    /* procfs */
    SIZEOF_INFO(psinfo_t),
    SIZEOF_INFO(prusage_t),
    SIZEOF_INFO(prcred_t),
    SIZEOF_INFO(pstatus_t),
    SIZEOF_INFO(struct proc),
    SIZEOF_INFO(struct user),
    {NULL, 0}
};

#endif

#ifdef hpux

#include <sys/pstat.h>
#include <mntent.h>
#include <sys/vfs.h>
#include <sys/mib.h>

static sizeof_info_t sizeof_info[] = {
    SIZEOF_INFO(struct pst_static),
    SIZEOF_INFO(struct pst_status),
    SIZEOF_INFO(struct pst_dynamic),
    SIZEOF_INFO(struct pst_swapinfo),
    SIZEOF_INFO(struct mntent),
    SIZEOF_INFO(struct statfs),
    SIZEOF_INFO(struct nmparms),
    SIZEOF_INFO(mib_ifEntry),
    SIZEOF_INFO(mib_ipRouteEnt),
    {NULL, 0}
};

#endif

static void print_sizeof_info(sizeof_info_t *info)
{
    while (info->name) {
        printf("sizeof(%s) == %d\n", info->name, info->size);
        ++info;
    }
}

int main(void) {
    system("uname -a");
    print_sizeof_info(sizeof_info);
    return 0;
}
