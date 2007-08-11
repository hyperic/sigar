/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

int sigar_get_multi_kstats(sigar_t *sigar,
                           kstat_list_t *kl,
                           const char *name,
                           kstat_t **retval)
{
    kstat_ctl_t *kc = sigar->kc;
    kstat_t *ksp;
    int i = 0;
    int dev;
    kid_t id = sigar_kstat_update(sigar);

    if (id == -1) {
        return errno;
    }

    name += kl->nlen; /* e.g. "hme0" + 3 */
    dev = atoi(name);

    if ((kl->num == 0) || (id > 0)) {
        while ((ksp = kstat_lookup(kc, kl->name, i, NULL))) {
            if (i+1 > kl->num) {
                kl->num = i+1;
                kl->ks = realloc(kl->ks, kl->num * sizeof(*kl->ks));
            }
            kl->ks[i] = ksp;
            i++;
        }
    }

    if (dev >= kl->num) {
        return ENXIO;
    }

    *retval = kl->ks[dev];

    return SIGAR_OK;
}

#define KSTAT_LIST_INIT(sigar, dev) \
    sigar->koffsets.dev[0] = -1; \
    sigar->ks.dev.num = 0; \
    sigar->ks.dev.ks  = NULL; \
    sigar->ks.dev.name = #dev; \
    sigar->ks.dev.nlen = sizeof(#dev)-1

void sigar_init_multi_kstats(sigar_t *sigar)
{
    KSTAT_LIST_INIT(sigar, lo);
    KSTAT_LIST_INIT(sigar, hme);
    KSTAT_LIST_INIT(sigar, dmfe);
    KSTAT_LIST_INIT(sigar, ge);
    KSTAT_LIST_INIT(sigar, eri);
}

void sigar_free_multi_kstats(sigar_t *sigar)
{
    if (sigar->ks.lo.num) {
        free(sigar->ks.lo.ks);
        KSTAT_LIST_INIT(sigar, lo);
    }
    if (sigar->ks.hme.num) {
        free(sigar->ks.hme.ks);
        KSTAT_LIST_INIT(sigar, hme);
    }
    if (sigar->ks.dmfe.num) {
        free(sigar->ks.dmfe.ks);
        KSTAT_LIST_INIT(sigar, dmfe);
    }
    if (sigar->ks.ge.num) {
        free(sigar->ks.ge.ks);
        KSTAT_LIST_INIT(sigar, ge);
    }
    if (sigar->ks.eri.num) {
        free(sigar->ks.eri.ks);
        KSTAT_LIST_INIT(sigar, eri);
    }
}

int sigar_get_kstats(sigar_t *sigar)
{
    kstat_ctl_t *kc = sigar->kc;
    unsigned int i, id, ncpu = sysconf(_SC_NPROCESSORS_CONF);
    int is_debug = SIGAR_LOG_IS_DEBUG(sigar);

    if (ncpu != sigar->ncpu) {
        if (!sigar->ks.lcpu) {
            /* init */
            sigar->ks.lcpu = ncpu;
            sigar->ks.cpu = malloc(sizeof(*(sigar->ks.cpu)) * ncpu);
            sigar->ks.cpu_info = malloc(sizeof(*(sigar->ks.cpu_info)) * ncpu);
            sigar->ks.cpuid = malloc(sizeof(*(sigar->ks.cpuid)) * ncpu);
        }
        else {
            sigar_log_printf(sigar, SIGAR_LOG_INFO,
                             "ncpu changed from %d to %d",
                             sigar->ncpu, ncpu);
            if (ncpu > sigar->ks.lcpu) {
                /* one or more cpus have been added */
                sigar->ks.cpu = realloc(sigar->ks.cpu,
                                        sizeof(*(sigar->ks.cpu)) * ncpu);
                sigar->ks.cpu_info = realloc(sigar->ks.cpu_info,
                                             sizeof(*(sigar->ks.cpu_info)) * ncpu);
                sigar->ks.cpuid = realloc(sigar->ks.cpuid,
                                          sizeof(*(sigar->ks.cpuid)) * ncpu);
                sigar->ks.lcpu = ncpu;
            }
            /* else or more cpus have been removed */
        }

        sigar->ncpu = ncpu;

        /* from man p_online:
         * ``Processor numbers are integers,
         *   greater than or equal to 0,
         *   and are defined by the hardware platform.
         *   Processor numbers are not necessarily contiguous,
         *   but "not too sparse."``
         * so we maintain our own mapping in ks.cpuid[]
         */

        /* lookup in order, which kstat chain may not be in */
        for (i=0, id=0; i<ncpu; id++) {
            kstat_t *cpu_info, *cpu_stat;

            if (!(cpu_info = kstat_lookup(kc, "cpu_info", id, NULL))) {
                continue;
            }

            if (!(cpu_stat = kstat_lookup(kc, "cpu_stat", id, NULL))) {
                /* XXX warn, faulted cpu? */
            }

            sigar->ks.cpu_info[i] = cpu_info;
            sigar->ks.cpu[i] = cpu_stat;
            sigar->ks.cpuid[i] = id;

            if (is_debug) {
                sigar_log_printf(sigar, SIGAR_LOG_DEBUG,
                                 "cpu %d id=%d", i, sigar->ks.cpuid[i]);
            }
            i++;
        }
    }
        
    sigar->ks.system   = kstat_lookup(kc, "unix", -1, "system_misc");
    sigar->ks.syspages = kstat_lookup(kc, "unix", -1, "system_pages");
    sigar->ks.mempages = kstat_lookup(kc, "bunyip", -1, "mempages");

    /* reset offsets on kstat_chain_update */
    sigar_free_multi_kstats(sigar);

    return SIGAR_OK;
}

SIGAR_INLINE kid_t sigar_kstat_update(sigar_t *sigar)
{
    kid_t id = kstat_chain_update(sigar->kc);

    switch (id) {
      case -1:
        sigar_log_printf(sigar, SIGAR_LOG_ERROR,
                         "kstat_chain_update error: %s",
                         sigar_strerror(sigar, errno));
        break;
      case 0:
        /* up-to-date */
        break;
      default:
        sigar_get_kstats(sigar);
        sigar_log(sigar, SIGAR_LOG_DEBUG,
                  "kstat chain updated");
        break;
    }

    return id;
}

/*
 * bincompat is not possible with certain kstat data structures between
 * solaris 2.6, 2.7, 2.8, etc.  alternative is to use kstat_data_lookup()
 * which means everytime we want a stat, must do a linear search
 * of ksp->ks_data.  eek.  so we meet half way and do the search for
 * each key once per sigar_t instance.  once the initial search has
 * been done, we have a table of offsets to quickly access the stats via
 * ksp->ks_data + offset.  this gives us bincompat without the overhead
 * of many kstat_data_lookup calls.
 */
static SIGAR_INLINE int kstat_named_offset(kstat_t *ksp, const char *name)
{
    unsigned int i;
    kstat_named_t *kn;

    for (i=0, kn=ksp->ks_data;
         i<ksp->ks_ndata;
         i++, kn++)
    {
        if (strEQ(kn->name, name)) {
            return i;
        }
    }

    return -2; /* not found */
}

static char *kstat_keys_lo[] = {
    "ipackets", /* RX_PACKETS */
    "opackets", /* TX_PACKETS */
    NULL
};

static char *kstat_keys_hme[] = {
    "ipackets", /* RX_PACKETS */
    "rbytes", /* RX_BYTES */
    "ierrors", /* RX_ERRORS */
    "missed", /* RX_DROPPED */
    "oflo", /* RX_OVERRUNS */
    "framing", /* RX_FRAME */
    "opackets", /* TX_PACKETS */
    "obytes", /* TX_BYTES */
    "oerrors", /* TX_ERRORS */
    "missed", /* TX_DROPPED */
    "oflo", /* TX_OVERRUNS */
    "collisions", /* TX_COLLISIONS */
    "nocarrier", /* TX_CARRIER */
    "ifspeed", /* SPEED */
    NULL
};

static char *kstat_keys_dmfe[] = {
    "ipackets", /* RX_PACKETS */
    "rbytes", /* RX_BYTES */
    "ierrors", /* RX_ERRORS */
    "missed", /* RX_DROPPED */
    "oflo", /* RX_OVERRUNS */
    "framing", /* RX_FRAME */
    "opackets", /* TX_PACKETS */
    "obytes", /* TX_BYTES */
    "oerrors", /* TX_ERRORS */
    "missed", /* TX_DROPPED */
    "oflo", /* TX_OVERRUNS */
    "collisions", /* TX_COLLISIONS */
    "nocarrier", /* TX_CARRIER */
    "ifspeed", /* SPEED */
    NULL
};

static char *kstat_keys_ge[] = {
    "ipackets", /* RX_PACKETS */
    "rbytes", /* RX_BYTES */
    "ierrors", /* RX_ERRORS */
    "drop", /* RX_DROPPED */
    "toolong_errors", /* RX_OVERRUNS */
    "framing", /* RX_FRAME */
    "opackets", /* TX_PACKETS */
    "obytes", /* TX_BYTES */
    "oerrors", /* TX_ERRORS */
    "drop", /* TX_DROPPED */
    "toolong_errors", /* TX_OVERRUNS */
    "collisions", /* TX_COLLISIONS */
    "nocarrier", /* TX_CARRIER */
    "ifspeed", /* SPEED */
    NULL
};

static char *kstat_keys_eri[] = {
    "ipackets", /* RX_PACKETS */
    "rbytes", /* RX_BYTES */
    "ierrors", /* RX_ERRORS */
    "drop", /* RX_DROPPED */
    "rx_overflow", /* RX_OVERRUNS */
    "parity_error", /* RX_FRAME */
    "opackets", /* TX_PACKETS */
    "obytes", /* TX_BYTES */
    "oerrors", /* TX_ERRORS */
    "drop", /* TX_DROPPED */
    "rx_overflow", /* TX_OVERRUNS */
    "collisions", /* TX_COLLISIONS */
    "nocarrier", /* TX_CARRIER */
    "ifspeed", /* SPEED */
    NULL
};

static char *kstat_keys_system[] = {
    "boot_time",
    "avenrun_1min",
    "avenrun_5min",
    "avenrun_15min",
    NULL
};

static char *kstat_keys_mempages[] = {
    "pages_anon",
    "pages_exec",
    "pages_vnode",
    NULL
};

static char *kstat_keys_syspages[] = {
    "pagesfree",
    NULL
};

static char **kstat_keys[] = {
    kstat_keys_lo,
    kstat_keys_hme,
    kstat_keys_dmfe,
    kstat_keys_ge,
    kstat_keys_eri,
    kstat_keys_system,
    kstat_keys_mempages,
    kstat_keys_syspages,
};

void sigar_koffsets_lookup(kstat_t *ksp, int *offsets, int kidx)
{
    int i;
    char **keys = kstat_keys[kidx];

    for (i=0; keys[i]; i++) {
        offsets[i] = kstat_named_offset(ksp, keys[i]);
    }
}

