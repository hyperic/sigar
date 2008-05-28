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

#ifndef SIGAR_FORMAT_H
#define SIGAR_FORMAT_H

typedef struct {
    double user;
    double sys;
    double nice;
    double idle;
    double wait;
    double irq;
    double soft_irq;
    double stolen;
    double combined;
} sigar_cpu_perc_t;

SIGAR_DECLARE(int) sigar_cpu_perc_calculate(sigar_cpu_t *prev,
                                            sigar_cpu_t *curr,
                                            sigar_cpu_perc_t *perc);

SIGAR_DECLARE(int) sigar_uptime_string(sigar_t *sigar, 
                                       sigar_uptime_t *uptime,
                                       char *buffer,
                                       int buflen);

SIGAR_DECLARE(char *) sigar_format_size(sigar_uint64_t size, char *buf);

SIGAR_DECLARE(int) sigar_net_address_equals(sigar_net_address_t *addr1,
                                            sigar_net_address_t *addr2);

SIGAR_DECLARE(int) sigar_net_address_to_string(sigar_t *sigar,
                                               sigar_net_address_t *address,
                                               char *addr_str);

SIGAR_DECLARE(sigar_uint32_t) sigar_net_address_hash(sigar_net_address_t *address);


SIGAR_DECLARE(const char *)sigar_net_connection_type_get(int type);

SIGAR_DECLARE(const char *)sigar_net_connection_state_get(int state);

SIGAR_DECLARE(char *) sigar_net_interface_flags_to_string(sigar_uint64_t flags, char *buf);

SIGAR_DECLARE(char *)sigar_net_services_name_get(sigar_t *sigar,
                                                 int protocol, unsigned long port);

#endif

