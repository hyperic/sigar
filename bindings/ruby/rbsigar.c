/*
 * Copyright (c) 2007-2009 Hyperic, Inc.
 * Copyright (c) 2009 SpringSource, Inc.
 * Copyright (c) 2009-2010 VMware, Inc.
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

#include <ruby.h>
#ifdef RB_RUBY_19
#include <ruby/re.h>
#else
#include <re.h>
#endif

#include <errno.h>
#include "sigar.h"
#include "sigar_fileinfo.h"
#include "sigar_log.h"
#include "sigar_format.h"
#include "sigar_ptql.h"

#define RB_SIGAR_RAISE(msg) rb_raise(rb_eArgError, "%s", msg)
#define RB_SIGAR_CROAK RB_SIGAR_RAISE(sigar_strerror(sigar, status))
#define OBJ2PID(pid) rb_sigar_pid_get(rbsigar, pid)

#ifndef RSTRING_PTR
#define RSTRING_PTR(s) RSTRING(s)->ptr
#endif

#ifndef RSTRING_LEN
#define RSTRING_LEN(s) RSTRING(s)->len
#endif

#ifdef RB_HAS_RE_ERROR
#  define RB_REGEX_ERROR rb_eRegexpError
#else
#  define RB_REGEX_ERROR rb_eArgError
#endif

#define SIGAR \
    sigar_t *sigar = rbsigar->sigar

#define SIGAR_GET \
    rb_sigar_t *rbsigar = rb_sigar_get(obj); \
    SIGAR

typedef struct {
    sigar_t *sigar;
    VALUE logger;
} rb_sigar_t;

static rb_sigar_t *rb_sigar_get(VALUE obj)
{
    rb_sigar_t *rbsigar;
    Data_Get_Struct(obj, rb_sigar_t, rbsigar);
    return rbsigar;
}

static int rbsigar_ptql_re_impl(void *data,
                                char *haystack, char *needle)
{
#ifdef RB_RUBY_19
    /* XXX no more regex.h */
    return 0;
#else
    struct re_pattern_buffer *regex;
    int len = strlen(haystack);
    int retval;
    const char *err;

    regex = ALLOC(struct re_pattern_buffer);
    MEMZERO((char *)regex, struct re_pattern_buffer, 1);
    /* XXX cache */
    if ((err = re_compile_pattern(needle, strlen(needle), regex))) {
        re_free_pattern(regex);
        rb_raise(RB_REGEX_ERROR, "%s", err);
        return 0;
    }

    retval = re_match(regex, haystack, len, 0, NULL);
    re_free_pattern(regex);
    return retval > 0;
#endif
}

#define sigar_isdigit(c) \
    (isdigit(((unsigned char)(c))))

static sigar_pid_t rb_sigar_pid_get(rb_sigar_t *rbsigar, VALUE obj)
{
    SIGAR;

    if (TYPE(obj) == T_STRING) {
        char *pid = StringValuePtr(obj);

        if (sigar_isdigit(*pid)) {
            obj = rb_str2inum(obj, 10);
            /* fallthru */
        }
        else if ((RSTRING_LEN(obj) == 2) &&
                 (*pid == '$') && (*(pid + 1) == '$'))
        {
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

                sigar_ptql_re_impl_set(sigar, NULL, rbsigar_ptql_re_impl);
                status = sigar_ptql_query_find_process(sigar, query, &qpid);
                sigar_ptql_re_impl_set(sigar, NULL, NULL);
                sigar_ptql_query_destroy(query);
                if (status == SIGAR_OK) {
                    return qpid;
                }
                else {
                    RB_SIGAR_RAISE(sigar_strerror(sigar, status));
                }
            }
            else {
                RB_SIGAR_RAISE(error.message);
            }
        }
    }
    return NUM2UINT(obj);
}

static void rb_sigar_free(void *obj)
{
    xfree(obj);
}

static void rb_sigar_close(rb_sigar_t *rbsigar)
{
    sigar_close(rbsigar->sigar);
    rb_sigar_free(rbsigar);
}

static void rb_sigar_mark(rb_sigar_t *rbsigar)
{
    rb_gc_mark(rbsigar->logger);
}

static VALUE rb_sigar_new(VALUE module)
{
    rb_sigar_t *rbsigar;
    rbsigar = ALLOC(rb_sigar_t);
    sigar_open(&(rbsigar->sigar));
    return Data_Wrap_Struct(module, rb_sigar_mark, rb_sigar_close, rbsigar);
}

static VALUE rb_sigar_format_size(VALUE rclass, VALUE size)
{
    char buffer[56];
    return rb_str_new2(sigar_format_size(NUM2LL(size), buffer));
}

static VALUE rb_sigar_net_interface_flags_to_s(VALUE rclass, VALUE flags)
{
    char buffer[1024];
    return rb_str_new2(sigar_net_interface_flags_to_string(NUM2LL(flags), buffer));
}

static VALUE rb_sigar_net_connection_type_to_s(VALUE rclass, VALUE type)
{
    return rb_str_new2(sigar_net_connection_type_get(NUM2INT(type)));
}

static VALUE rb_sigar_net_connection_state_to_s(VALUE rclass, VALUE state)
{
    return rb_str_new2(sigar_net_connection_state_get(NUM2INT(state)));
}

static VALUE rb_sigar_net_address_to_string(sigar_net_address_t *address)
{
    char addr_str[SIGAR_INET6_ADDRSTRLEN];
    sigar_net_address_to_string(NULL, address, addr_str);
    return rb_str_new2(addr_str);
}

static VALUE rb_sigar_net_scope_to_s(VALUE rclass, VALUE type)
{
    return rb_str_new2(sigar_net_scope_to_string(NUM2INT(type)));
}

#define rb_sigar_net_address_to_s(a) rb_sigar_net_address_to_string(&a) 

static VALUE rb_sigar_new_list(char *data, unsigned long number,
                               int size, VALUE rclass)
{
    unsigned long i;
    VALUE av = rb_ary_new2(number);

    for (i=0; i<number; i++, data += size) {
        void *ent = malloc(size);

        memcpy(ent, data, size);
        rb_ary_push(av, Data_Wrap_Struct(rclass, 0, free, ent));
    }

    return av;
}

static VALUE rb_sigar_new_strlist(char **data, unsigned long number)
{
    unsigned long i;
    VALUE av = rb_ary_new2(number);

    for (i=0; i<number; i++) {
        rb_ary_push(av, rb_str_new2(data[i]));
    }

    return av;
}

static VALUE rb_sigar_new_intlist(int *data, int number)
{
    int i;
    VALUE av = rb_ary_new2(number);

    for (i=0; i<number; i++) {
        rb_ary_push(av, rb_int2inum(data[i]));
    }

    return av;
}

static VALUE rb_sigar_new_doublelist(double *data, int number)
{
    int i;
    VALUE av = rb_ary_new2(number);

    for (i=0; i<number; i++) {
        rb_ary_push(av, rb_float_new(data[i]));
    }

    return av;
}

static VALUE rb_sigar_net_interface_list(VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_net_interface_list_t iflist;
    VALUE RETVAL;

    status = sigar_net_interface_list_get(sigar, &iflist);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_strlist(iflist.data, iflist.number);

    sigar_net_interface_list_destroy(sigar, &iflist);

    return RETVAL;
}

static int rb_sigar_str2net_address(VALUE bytes, sigar_net_address_t *address)
{
    long len = RSTRING_LEN(bytes);

    switch (len) {
      case 4:
        address->family = SIGAR_AF_INET;
        break;
      case 4*4:
        address->family = SIGAR_AF_INET6;
        break;
      default:
        return EINVAL;
    }

    memcpy(RSTRING_PTR(bytes), &address->addr.in6, len);

    return SIGAR_OK;
}

static VALUE rb_cSigarNetStat;

static VALUE rb_sigar_net_stat_get(VALUE obj, VALUE flags, VALUE bytes, int port)
{
    SIGAR_GET;

    int status;
    int has_port = (port != -1);
    sigar_net_stat_t *RETVAL = malloc(sizeof(*RETVAL));
    sigar_net_address_t address;

    if (has_port) {
        status = rb_sigar_str2net_address(bytes, &address);
        if (status == SIGAR_OK) {
            status = sigar_net_stat_port_get(sigar, RETVAL, NUM2INT(flags),
                                             &address, port);
        }
    }
    else {
        status = sigar_net_stat_get(sigar, RETVAL, NUM2INT(flags));
    }

    if (status != SIGAR_OK) {
        free(RETVAL);
        RB_SIGAR_CROAK;
    }

    return Data_Wrap_Struct(rb_cSigarNetStat, 0, rb_sigar_free, RETVAL);
}

static VALUE rb_sigar_net_stat(VALUE obj, VALUE flags)
{
    return rb_sigar_net_stat_get(obj, flags, Qnil, -1);
}

static VALUE rb_sigar_net_stat_port(VALUE obj, VALUE flags, VALUE address, VALUE port)
{
    return rb_sigar_net_stat_get(obj, flags, address, NUM2INT(port));
}

static VALUE rb_sigar_NetStat_tcp_states(VALUE self)
{
    sigar_net_stat_t *net_stat;

    Data_Get_Struct(self, sigar_net_stat_t, net_stat);

    return rb_sigar_new_intlist(&net_stat->tcp_states[0], SIGAR_TCP_UNKNOWN);
}

static VALUE rb_cSigarNetConnection;

static VALUE rb_sigar_net_connection_list(VALUE obj, VALUE flags)
{
    SIGAR_GET;

    int status;
    sigar_net_connection_list_t connlist;
    VALUE RETVAL;

    status = sigar_net_connection_list_get(sigar, &connlist, NUM2UINT(flags));

    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_list((char *)&connlist.data[0],
                               connlist.number,
                               sizeof(*connlist.data),
                               rb_cSigarNetConnection);

    sigar_net_connection_list_destroy(sigar, &connlist);

    return RETVAL;
}

static VALUE rb_sigar_net_services_name(VALUE obj, VALUE protocol, VALUE port)
{
    SIGAR_GET;

    char *name;

    if ((name = sigar_net_services_name_get(sigar, NUM2UINT(protocol), NUM2UINT(port)))) {
        return rb_str_new2(name);
    }
    else {
        return Qnil;
    }
}

static VALUE rb_cSigarCpuInfo;

static VALUE rb_sigar_cpu_info_list(VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_cpu_info_list_t cpu_infos;
    VALUE RETVAL;

    status = sigar_cpu_info_list_get(sigar, &cpu_infos);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_list((char *)&cpu_infos.data[0],
                               cpu_infos.number,
                               sizeof(*cpu_infos.data),
                               rb_cSigarCpuInfo);

    sigar_cpu_info_list_destroy(sigar, &cpu_infos);

    return RETVAL;
}

static VALUE rb_sigar_loadavg(VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_loadavg_t loadavg;

    status = sigar_loadavg_get(sigar, &loadavg);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    return rb_sigar_new_doublelist(&loadavg.loadavg[0], 3);
}

static VALUE rb_cSigarCpuPerc;

static VALUE rb_cSigarFileSystem;

static VALUE rb_sigar_file_system_list(VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_file_system_list_t fslist;
    VALUE RETVAL;

    status = sigar_file_system_list_get(sigar, &fslist);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_list((char *)&fslist.data[0],
                               fslist.number,
                               sizeof(*fslist.data),
                               rb_cSigarFileSystem);

    sigar_file_system_list_destroy(sigar, &fslist);

    return RETVAL;
}

static VALUE rb_cSigarArp;

static VALUE rb_sigar_arp_list(VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_arp_list_t list;
    VALUE RETVAL;

    status = sigar_arp_list_get(sigar, &list);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_list((char *)&list.data[0],
                               list.number,
                               sizeof(*list.data),
                               rb_cSigarArp);

    sigar_arp_list_destroy(sigar, &list);

    return RETVAL;
}

static VALUE rb_cSigarWho;

static VALUE rb_sigar_who_list(VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_who_list_t list;
    VALUE RETVAL;

    status = sigar_who_list_get(sigar, &list);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_list((char *)&list.data[0],
                               list.number,
                               sizeof(*list.data),
                               rb_cSigarWho);

    sigar_who_list_destroy(sigar, &list);

    return RETVAL;
}

static VALUE rb_cSigarNetRoute;

static VALUE rb_sigar_net_route_list(VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_net_route_list_t list;
    VALUE RETVAL;

    status = sigar_net_route_list_get(sigar, &list);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_list((char *)&list.data[0],
                               list.number,
                               sizeof(*list.data),
                               rb_cSigarNetRoute);

    sigar_net_route_list_destroy(sigar, &list);

    return RETVAL;
}

static VALUE rb_sigar_proc_list(int argc, VALUE *argv, VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_proc_list_t list;
    VALUE RETVAL;
    VALUE vptql;

    rb_scan_args(argc, argv, "01", &vptql);
    if (NIL_P(vptql)) {
        status = sigar_proc_list_get(sigar, &list);

        if (status != SIGAR_OK) {
            RB_SIGAR_CROAK;
        }
    }
    else {
        sigar_ptql_query_t *query;
        sigar_ptql_error_t error;
        char *ptql = StringValuePtr(vptql);

        status = sigar_ptql_query_create(&query, ptql, &error);

        if (status != SIGAR_OK) {
            RB_SIGAR_RAISE(error.message);
        }
        sigar_ptql_re_impl_set(sigar, NULL, rbsigar_ptql_re_impl);
        status = sigar_ptql_query_find(sigar, query, &list);
        sigar_ptql_re_impl_set(sigar, NULL, NULL);
        sigar_ptql_query_destroy(query);
        if (status != SIGAR_OK) {
            RB_SIGAR_RAISE(sigar_strerror(sigar, status));
        }
    }

    RETVAL = rb_sigar_new_intlist(&list.data[0],
                                  list.number);

    sigar_proc_list_destroy(sigar, &list);

    return RETVAL;
}

static VALUE rb_sigar_proc_args(VALUE obj, VALUE pid)
{
    SIGAR_GET;

    int status;
    sigar_proc_args_t args;
    VALUE RETVAL;

    status = sigar_proc_args_get(sigar, OBJ2PID(pid), &args);

    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_strlist(args.data, args.number);

    sigar_proc_args_destroy(sigar, &args);

    return RETVAL;
}

static int rb_sigar_env_getall(void *data,
                               const char *key, int klen,
                               char *val, int vlen)
{
    rb_hash_aset(*((VALUE*)data),
                 rb_str_new(key, klen),
                 rb_str_new(val, vlen));
    return SIGAR_OK;
}

static VALUE rb_sigar_proc_port(VALUE obj, VALUE protocol, VALUE port)
{
    SIGAR_GET;

    int status;
    sigar_pid_t RETVAL;

    status = sigar_proc_port_get(sigar, NUM2INT(protocol), NUM2LL(port), &RETVAL);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    return rb_int2inum(RETVAL);
}

static VALUE rb_sigar_proc_env(VALUE obj, VALUE pid)
{
    SIGAR_GET;

    int status;
    sigar_proc_env_t procenv;
    VALUE RETVAL = rb_hash_new();

    procenv.type = SIGAR_PROC_ENV_ALL;
    procenv.env_getter = rb_sigar_env_getall;
    procenv.data = &RETVAL;

    status = sigar_proc_env_get(sigar, OBJ2PID(pid), &procenv);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    return RETVAL;
}

VALUE rb = Qnil;

static const char *logger_consts[] = {
    "FATAL", /* SIGAR_LOG_FATAL */
    "ERROR", /* SIGAR_LOG_ERROR */
    "WARN",  /* SIGAR_LOG_WARN */
    "INFO",  /* SIGAR_LOG_INFO */
    "DEBUG", /* SIGAR_LOG_DEBUG */
    "DEBUG", /* SIGAR_LOG_TRACE */
};

static void rb_sigar_logger_impl(sigar_t *sigar, void *data,
                            int level, char *message)
{
    rb_sigar_t *rbsigar = ((rb_sigar_t*)data);
    VALUE logger = rbsigar->logger;

    /* XXX: cost of this, better way? */
    VALUE logger_const = rb_const_get(rb_cObject, rb_intern("Logger"));
    VALUE logger_level = rb_const_get(logger_const, 
                                          rb_intern(logger_consts[level]));
    VALUE msg = rb_str_new2(message);

    rb_funcall(logger, rb_intern ("add"), 2, logger_level, msg);

    return;
}

static void rb_sigar_proc_impl(sigar_t *sigar, void *data,
                            int level, char *message)
{
    rb_sigar_t *rbsigar = ((rb_sigar_t*)data);
    VALUE logger = rbsigar->logger;

    rb_funcall(logger, rb_intern("call"), 2, INT2FIX(level), rb_str_new2(message));

    return;
}

static VALUE rb_sigar_logger(VALUE obj)
{
  rb_sigar_t *rbsigar = rb_sigar_get(obj);

  return rbsigar->logger;
}

static VALUE rb_sigar_set_logger(VALUE obj, VALUE logger)
{
    SIGAR_GET;

    if (rb_obj_is_kind_of(logger, rb_cProc) || 
        rb_respond_to(logger, rb_intern("call"))) {

        sigar_log_impl_set(sigar, rbsigar, rb_sigar_proc_impl);
        rbsigar->logger = logger;

        return obj;
    } 

    /* Have to load Logger to test for it properly */
    rb_require("logger");
    if (rb_obj_is_kind_of(logger, rb_path2class("Logger"))) {
        sigar_log_impl_set(sigar, rbsigar, rb_sigar_logger_impl);
        rbsigar->logger = logger;
    }
    else {
        rb_raise(rb_eArgError, 
                 "value is not a proc object or subclass of Logger");
    }

    return obj;
}

static VALUE rb_sigar_log_level(VALUE obj)
{
    SIGAR_GET;

    return INT2FIX(sigar_log_level_get(sigar));
}

static VALUE rb_sigar_set_log_level(VALUE obj, VALUE level)
{
    SIGAR_GET;

    sigar_log_level_set(sigar, NUM2INT(level));

    return obj;
}

static VALUE rb_sigar_fqdn(VALUE obj)
{
    SIGAR_GET;

    char fqdn[SIGAR_FQDN_LEN];
    int status;

    if ((status = sigar_fqdn_get(sigar, fqdn, sizeof(fqdn))) != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    return rb_str_new2(fqdn);
}

#include "./rbsigar_generated.rx"

static VALUE rb_sigar_cpu_list(VALUE obj)
{
    SIGAR_GET;

    int status;
    sigar_cpu_list_t cpus;
    VALUE RETVAL;

    status = sigar_cpu_list_get(sigar, &cpus);
    if (status != SIGAR_OK) {
        RB_SIGAR_CROAK;
    }

    RETVAL = rb_sigar_new_list((char *)&cpus.data[0],
                               cpus.number,
                               sizeof(*cpus.data),
                               rb_cSigarCpu);

    sigar_cpu_list_destroy(sigar, &cpus);

    return RETVAL;
}

#define RB_SIGAR_CONST_INT(name) \
    rb_define_const(rclass, #name, INT2FIX(SIGAR_##name))

#define RB_SIGAR_DEFINE_CONST_STR(name, value) \
    rb_define_const(rclass, name, rb_obj_freeze(rb_str_new2(value)))

#define RB_SIGAR_CONST_STR(name) \
    rb_define_const(rclass, #name, rb_obj_freeze(rb_str_new2(SIGAR_##name)))

static void Init_rbsigar_constants(VALUE rclass)
{
    RB_SIGAR_CONST_INT(FSTYPE_NONE);
    RB_SIGAR_CONST_INT(FSTYPE_LOCAL_DISK);
    RB_SIGAR_CONST_INT(FSTYPE_NETWORK);
    RB_SIGAR_CONST_INT(FSTYPE_RAM_DISK);
    RB_SIGAR_CONST_INT(FSTYPE_CDROM);
    RB_SIGAR_CONST_INT(FSTYPE_SWAP);

    RB_SIGAR_CONST_INT(IFF_UP);
    RB_SIGAR_CONST_INT(IFF_BROADCAST);
    RB_SIGAR_CONST_INT(IFF_DEBUG);
    RB_SIGAR_CONST_INT(IFF_LOOPBACK);
    RB_SIGAR_CONST_INT(IFF_POINTOPOINT);
    RB_SIGAR_CONST_INT(IFF_NOTRAILERS);
    RB_SIGAR_CONST_INT(IFF_RUNNING);
    RB_SIGAR_CONST_INT(IFF_NOARP);
    RB_SIGAR_CONST_INT(IFF_PROMISC);
    RB_SIGAR_CONST_INT(IFF_ALLMULTI);
    RB_SIGAR_CONST_INT(IFF_MULTICAST);
    RB_SIGAR_CONST_INT(IFF_SLAVE);
    RB_SIGAR_CONST_INT(IFF_MASTER);
    RB_SIGAR_CONST_INT(IFF_DYNAMIC);

    RB_SIGAR_CONST_INT(NETCONN_CLIENT);
    RB_SIGAR_CONST_INT(NETCONN_SERVER);
    RB_SIGAR_CONST_INT(NETCONN_TCP);
    RB_SIGAR_CONST_INT(NETCONN_UDP);
    RB_SIGAR_CONST_INT(NETCONN_RAW);
    RB_SIGAR_CONST_INT(NETCONN_UNIX);

    RB_SIGAR_CONST_INT(TCP_ESTABLISHED);
    RB_SIGAR_CONST_INT(TCP_SYN_SENT);
    RB_SIGAR_CONST_INT(TCP_SYN_RECV);
    RB_SIGAR_CONST_INT(TCP_FIN_WAIT1);
    RB_SIGAR_CONST_INT(TCP_FIN_WAIT2);
    RB_SIGAR_CONST_INT(TCP_TIME_WAIT);
    RB_SIGAR_CONST_INT(TCP_CLOSE);
    RB_SIGAR_CONST_INT(TCP_CLOSE_WAIT);
    RB_SIGAR_CONST_INT(TCP_LAST_ACK);
    RB_SIGAR_CONST_INT(TCP_LISTEN);
    RB_SIGAR_CONST_INT(TCP_CLOSING);
    RB_SIGAR_CONST_INT(TCP_IDLE);
    RB_SIGAR_CONST_INT(TCP_BOUND);
    RB_SIGAR_CONST_INT(TCP_UNKNOWN);

    RB_SIGAR_CONST_INT(RTF_UP);
    RB_SIGAR_CONST_INT(RTF_GATEWAY);
    RB_SIGAR_CONST_INT(RTF_HOST);

    RB_SIGAR_CONST_STR(NULL_HWADDR);

    RB_SIGAR_CONST_INT(LOG_FATAL);
    RB_SIGAR_CONST_INT(LOG_ERROR);
    RB_SIGAR_CONST_INT(LOG_WARN);
    RB_SIGAR_CONST_INT(LOG_INFO);
    RB_SIGAR_CONST_INT(LOG_DEBUG);
    RB_SIGAR_CONST_INT(LOG_TRACE);
}

static void Init_rbsigar_version(VALUE rclass)
{
    sigar_version_t *sv = sigar_version_get();
    RB_SIGAR_DEFINE_CONST_STR("BUILD_DATE", sv->build_date);
    RB_SIGAR_DEFINE_CONST_STR("SCM_REVISION", sv->scm_revision);
    RB_SIGAR_DEFINE_CONST_STR("VERSION", sv->version);
}

void Init_sigar(void)
{
    VALUE rclass = rb_define_class("Sigar", rb_cObject);

    rb_define_method(rclass, "logger", rb_sigar_logger, 0);
    rb_define_method(rclass, "logger=", rb_sigar_set_logger, 1);

    rb_define_method(rclass, "log_level", rb_sigar_log_level, 0);
    rb_define_method(rclass, "log_level=", rb_sigar_set_log_level, 1);

    rb_define_method(rclass, "cpu_info_list", rb_sigar_cpu_info_list, 0);
    rb_define_method(rclass, "cpu_list", rb_sigar_cpu_list, 0);
    rb_define_method(rclass, "loadavg", rb_sigar_loadavg, 0);
    rb_define_method(rclass, "file_system_list", rb_sigar_file_system_list, 0);
    rb_define_method(rclass, "net_connection_list", rb_sigar_net_connection_list, 1);
    rb_define_method(rclass, "net_interface_list", rb_sigar_net_interface_list, 0);
    rb_define_method(rclass, "net_services_name", rb_sigar_net_services_name, 2);
    rb_define_method(rclass, "net_stat", rb_sigar_net_stat, 1);
    rb_define_method(rclass, "net_stat_port", rb_sigar_net_stat_port, 3);
    rb_define_method(rclass, "net_route_list", rb_sigar_net_route_list, 0);
    rb_define_method(rclass, "arp_list", rb_sigar_arp_list, 0);
    rb_define_method(rclass, "who_list", rb_sigar_who_list, 0);
    rb_define_method(rclass, "proc_list", rb_sigar_proc_list, -1);
    rb_define_method(rclass, "proc_args", rb_sigar_proc_args, 1);
    rb_define_method(rclass, "proc_env", rb_sigar_proc_env, 1);
    rb_define_method(rclass, "proc_port", rb_sigar_proc_port, 2);
    rb_define_method(rclass, "fqdn", rb_sigar_fqdn, 0);

    rb_define_singleton_method(rclass, "new", rb_sigar_new, 0);
    rb_define_singleton_method(rclass, "format_size", rb_sigar_format_size, 1);
    rb_define_singleton_method(rclass, "net_interface_flags_to_s",
                               rb_sigar_net_interface_flags_to_s, 1);
    rb_define_singleton_method(rclass, "net_scope_to_s",
                               rb_sigar_net_scope_to_s, 1);
    rb_define_singleton_method(rclass, "net_connection_type_to_s",
                               rb_sigar_net_connection_type_to_s, 1);
    rb_define_singleton_method(rclass, "net_connection_state_to_s",
                               rb_sigar_net_connection_state_to_s, 1);

    Init_rbsigar_constants(rclass);
    Init_rbsigar_version(rclass);

    /* generated */
    rb_sigar_define_module_methods(rclass);
    rb_define_method(rb_cSigarNetStat, "tcp_states", rb_sigar_NetStat_tcp_states, 0);
}
