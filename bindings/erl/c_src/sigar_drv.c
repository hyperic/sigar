/*
 * Copyright (c) 2009 SpringSource, Inc.
 * Copyright (c) 2010 VMware, Inc.
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

#include <erl_driver.h>
#include <ei.h>
#include <ctype.h>
#include <string.h>

#include "sigar.h"
#include "sigar_fileinfo.h"
#include "sigar_format.h"
#include "sigar_ptql.h"

typedef struct {
    ErlDrvPort port;
    sigar_t *sigar;
} sigar_drv_t;

static ErlDrvData start(ErlDrvPort port, char *cmd) {
    sigar_drv_t *sd = (sigar_drv_t *)driver_alloc(sizeof(*sd));
    int status;

    status = sigar_open(&sd->sigar);
    if (status != SIGAR_OK) {
        sd->sigar = NULL;
        driver_failure_posix(port, status);
    }

    sd->port = port;

    return (ErlDrvData)sd;
}

static void stop(ErlDrvData handle) {
    sigar_drv_t *driver_data = (sigar_drv_t *)handle;
    if (driver_data->sigar) {
        sigar_close(driver_data->sigar);
    }
}

#ifdef SIGAR_64BIT
#define str2pid(value) strtoull(value, NULL, 10)
#else
#define str2pid(value) strtoul(value, NULL, 10)
#endif

static sigar_pid_t esigar_pid_get(sigar_t *sigar, char *pid)
{
    if (isdigit(*pid)) {
        return str2pid(pid);
    }
    else if ((*pid == '$') && (*(pid + 1) == '$')) {
        return sigar_pid_get(sigar);
    }
    else {
        /* XXX cache queries */
        sigar_ptql_query_t *query;
        sigar_ptql_error_t error;
        int status =
            sigar_ptql_query_create(&query, (char *)pid, &error);

        if (status == SIGAR_OK) {
            sigar_pid_t qpid;

            status = sigar_ptql_query_find_process(sigar, query, &qpid);
            sigar_ptql_query_destroy(query);
            if (status == SIGAR_OK) {
                return qpid;
            }
        }
    }
    return 0;
}

static void esigar_to_strlist(ei_x_buff *x,
                              char **data, unsigned long number)
{
    unsigned long i;

    ei_x_encode_list_header(x, number);
    for (i=0; i<number; i++) {
        ei_x_encode_string(x, data[i]);
    }
    ei_x_encode_empty_list(x);
}

typedef void (*esigar_encoder_func_t)(ei_x_buff *x, void *data);

static void esigar_to_list(ei_x_buff *x,
                           char *data, unsigned long number, int size,
                           esigar_encoder_func_t encoder)
{
    unsigned long i;

    ei_x_encode_list_header(x, number);
    for (i=0; i<number; i++, data += size) {
        encoder(x, data);
    }
    ei_x_encode_empty_list(x);
}

#define ESIGAR_NEW(x) \
    ei_x_new_with_version(x)

#define ESIGAR_OK(x) \
    ei_x_encode_tuple_header(x, 2); \
    ei_x_encode_atom(x, "ok")

#define ESIGAR_ERROR(x, sigar, status) \
    ei_x_encode_tuple_header(x, 2); \
    ei_x_encode_atom(x, "error"); \
    ei_x_encode_string(x, sigar_strerror(sigar, status))

#define ESIGAR_SEND(p, x) \
    driver_output(port, (x)->buff, (x)->index); \
    ei_x_free(x)

#define esigar_encode_long(x, k, v) \
    ei_x_encode_tuple_header(x, 2); \
    ei_x_encode_atom(x, k); \
    ei_x_encode_long(x, v)

#define esigar_encode_ulonglong(x, k, v) \
    ei_x_encode_tuple_header(x, 2); \
    ei_x_encode_atom(x, k); \
    ei_x_encode_ulonglong(x, v)

#define esigar_encode_char(x, k, v) \
    ei_x_encode_tuple_header(x, 2); \
    ei_x_encode_atom(x, k); \
    ei_x_encode_char(x, v)

#define esigar_encode_string(x, k, v) \
    ei_x_encode_tuple_header(x, 2); \
    ei_x_encode_atom(x, k); \
    ei_x_encode_string(x, v)

#define esigar_encode_double(x, k, v) \
    ei_x_encode_tuple_header(x, 2); \
    ei_x_encode_atom(x, k); \
    ei_x_encode_double(x, v)

static void esigar_encode_net_address(ei_x_buff *x, const char *key,
                                      sigar_net_address_t *address)
{
    char buf[SIGAR_INET6_ADDRSTRLEN];
    sigar_net_address_to_string(NULL, address, buf);
    esigar_encode_string(x, key, buf);
}

#define esigar_encode_netaddr(x, k, v) \
    esigar_encode_net_address(x, k, &v)

static void esigar_notimpl(ErlDrvPort port, sigar_t *sigar, int cmd)
{
    ei_x_buff x;

    ESIGAR_NEW(&x);
    ESIGAR_ERROR(&x, sigar, SIGAR_ENOTIMPL);
    ESIGAR_SEND(port, &x);
}

#include "../priv/gen/sigar_drv_gen.c"

static void esigar_loadavg_get(ErlDrvPort port, sigar_t *sigar)
{
    int status;
    ei_x_buff x;
    sigar_loadavg_t loadavg;

    ESIGAR_NEW(&x);

    if ((status = sigar_loadavg_get(sigar, &loadavg)) == SIGAR_OK) {
        ESIGAR_OK(&x);

        ei_x_encode_list_header(&x, 3);
        ei_x_encode_double(&x, loadavg.loadavg[0]);
        ei_x_encode_double(&x, loadavg.loadavg[1]);
        ei_x_encode_double(&x, loadavg.loadavg[2]);
        ei_x_encode_empty_list(&x);
    }
    else {
        ESIGAR_ERROR(&x, sigar, status);
    }

    ESIGAR_SEND(port, &x);
}

static void esigar_net_connection_list_get(ErlDrvPort port, sigar_t *sigar,
                                           unsigned int flags)
{
    int status;
    ei_x_buff x;
    sigar_net_connection_list_t list;

    ei_x_new_with_version(&x);

    if ((status = sigar_net_connection_list_get(sigar, &list, flags) == SIGAR_OK)) {
        ESIGAR_OK(&x);

        esigar_to_list(&x,
                       (char *)&list.data[0], list.number,
                       sizeof(*list.data),
                       (esigar_encoder_func_t)esigar_encode_net_connection);

        sigar_net_connection_list_destroy(sigar, &list);
    }
    else {
        ESIGAR_ERROR(&x, sigar, status);
    }

    ESIGAR_SEND(port, &x);
}

static void esigar_net_interface_list_get(ErlDrvPort port, sigar_t *sigar)
{
    int status;
    ei_x_buff x;
    sigar_net_interface_list_t list;

    ei_x_new_with_version(&x);

    if ((status = sigar_net_interface_list_get(sigar, &list) == SIGAR_OK)) {
        ESIGAR_OK(&x);

        esigar_to_strlist(&x, list.data, list.number);
        sigar_net_interface_list_destroy(sigar, &list);
    }
    else {
        ESIGAR_ERROR(&x, sigar, status);
    }

    ESIGAR_SEND(port, &x);
}

static void esigar_file_system_list_get(ErlDrvPort port, sigar_t *sigar)
{
    int status;
    ei_x_buff x;
    sigar_file_system_list_t list;

    ei_x_new_with_version(&x);

    if ((status = sigar_file_system_list_get(sigar, &list) == SIGAR_OK)) {
        ESIGAR_OK(&x);

        esigar_to_list(&x,
                       (char *)&list.data[0], list.number,
                       sizeof(*list.data),
                       (esigar_encoder_func_t)esigar_encode_file_system);

        sigar_file_system_list_destroy(sigar, &list);
    }
    else {
        ESIGAR_ERROR(&x, sigar, status);
    }

    ESIGAR_SEND(port, &x);
}

static void esigar_net_route_list_get(ErlDrvPort port, sigar_t *sigar)
{
    int status;
    ei_x_buff x;
    sigar_net_route_list_t list;

    ei_x_new_with_version(&x);

    if ((status = sigar_net_route_list_get(sigar, &list) == SIGAR_OK)) {
        ESIGAR_OK(&x);

        esigar_to_list(&x,
                       (char *)&list.data[0], list.number,
                       sizeof(*list.data),
                       (esigar_encoder_func_t)esigar_encode_net_route);

        sigar_net_route_list_destroy(sigar, &list);
    }
    else {
        ESIGAR_ERROR(&x, sigar, status);
    }

    ESIGAR_SEND(port, &x);
}

static void esigar_cpu_info_list_get(ErlDrvPort port, sigar_t *sigar)
{
    int status;
    ei_x_buff x;
    sigar_cpu_info_list_t list;

    ei_x_new_with_version(&x);

    if ((status = sigar_cpu_info_list_get(sigar, &list) == SIGAR_OK)) {
        ESIGAR_OK(&x);

        esigar_to_list(&x,
                       (char *)&list.data[0], list.number,
                       sizeof(*list.data),
                       (esigar_encoder_func_t)esigar_encode_cpu_info);

        sigar_cpu_info_list_destroy(sigar, &list);
    }
    else {
        ESIGAR_ERROR(&x, sigar, status);
    }

    ESIGAR_SEND(port, &x);
}

static void esigar_arp_list_get(ErlDrvPort port, sigar_t *sigar)
{
    int status;
    ei_x_buff x;
    sigar_arp_list_t list;

    ei_x_new_with_version(&x);

    if ((status = sigar_arp_list_get(sigar, &list) == SIGAR_OK)) {
        ESIGAR_OK(&x);

        esigar_to_list(&x,
                       (char *)&list.data[0], list.number,
                       sizeof(*list.data),
                       (esigar_encoder_func_t)esigar_encode_arp);

        sigar_arp_list_destroy(sigar, &list);
    }
    else {
        ESIGAR_ERROR(&x, sigar, status);
    }

    ESIGAR_SEND(port, &x);
}

static void esigar_who_list_get(ErlDrvPort port, sigar_t *sigar)
{
    int status;
    ei_x_buff x;
    sigar_who_list_t list;

    ei_x_new_with_version(&x);

    if ((status = sigar_who_list_get(sigar, &list) == SIGAR_OK)) {
        ESIGAR_OK(&x);

        esigar_to_list(&x,
                       (char *)&list.data[0], list.number,
                       sizeof(*list.data),
                       (esigar_encoder_func_t)esigar_encode_who);

        sigar_who_list_destroy(sigar, &list);
    }
    else {
        ESIGAR_ERROR(&x, sigar, status);
    }

    ESIGAR_SEND(port, &x);
}

static void outputv(ErlDrvData handle, ErlIOVec *ev) {
    sigar_drv_t *sd = (sigar_drv_t *)handle;
    sigar_t *sigar = sd->sigar;
    ErlDrvPort port = sd->port;
    ErlDrvBinary *data = ev->binv[1];
    int cmd = data->orig_bytes[0];

    switch(cmd) {
    case ESIGAR_NET_CONNECTION_LIST:
        esigar_net_connection_list_get(port, sigar,
                                       data->orig_bytes[1]);
        break;
    case ESIGAR_NET_INTERFACE_LIST:
        esigar_net_interface_list_get(port, sigar);
        break;
    case ESIGAR_NET_ROUTE_LIST:
        esigar_net_route_list_get(port, sigar);
        break;
    case ESIGAR_FILE_SYSTEM_LIST:
        esigar_file_system_list_get(port, sigar);
        break;
    case ESIGAR_CPU_INFO_LIST:
        esigar_cpu_info_list_get(port, sigar);
        break;
    case ESIGAR_ARP_LIST:
        esigar_arp_list_get(port, sigar);
        break;
    case ESIGAR_WHO_LIST:
        esigar_who_list_get(port, sigar);
        break;
    case ESIGAR_LOADAVG:
        esigar_loadavg_get(port, sigar);
        break;
    default:
        esigar_dispatch(port, sigar, cmd, &data->orig_bytes[1]);
        break;
    }
}

static ErlDrvEntry sigar_driver_entry = {
    NULL,                             /* init */
    start,                            /* startup */
    stop,                             /* shutdown */
    NULL,                             /* output */
    NULL,                             /* ready_input */
    NULL,                             /* ready_output */
    "sigar_drv",                      /* name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    outputv,                          /* outputv */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};

DRIVER_INIT(sigar_driver) {
    return &sigar_driver_entry;
}
