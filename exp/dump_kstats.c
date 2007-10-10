#include <stdio.h>
#include <kstat.h>

/* gcc -lkstat -o dump_kstats dump_kstats.c */

static const char *kstat_type_names[] = {
    "raw", "named", "intr", "io", "timer"
};

int main(int argc, char **argv) {
    kstat_ctl_t *kc = kstat_open();
    kstat_t *kp;

    for (kp = kc->kc_chain; kp != 0; kp = kp->ks_next) {
        if (strncmp(kp->ks_name, "kstat_", 6) == 0) {
            continue;
        }
        fprintf(stdout, "%-5s %s::%s.%s[%d]\n",
                kstat_type_names[kp->ks_type],
                kp->ks_class, kp->ks_module, kp->ks_name, kp->ks_instance);
    }

    kstat_close(kc);
}
