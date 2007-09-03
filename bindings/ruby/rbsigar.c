#include <ruby.h>
#include "sigar.h"
#include "sigar_fileinfo.h"
#include "sigar_format.h"

#define RB_SIGAR_CROAK rb_raise(rb_eArgError, "%s", sigar_strerror(sigar, status))

static sigar_t *rb_sigar_get(VALUE obj)
{
    sigar_t *sigar;
    Data_Get_Struct(obj, sigar_t, sigar);
    return sigar;
}

static void rb_sigar_free(void *obj)
{
    free(obj);
}

static void rb_sigar_close(void *obj)
{
    sigar_close((sigar_t *)obj);
}

static VALUE rb_sigar_new(VALUE module)
{
    sigar_t *sigar;
    sigar_open(&sigar);
    return Data_Wrap_Struct(module, 0, rb_sigar_close, sigar);
}

static VALUE rb_sigar_format_size(VALUE rclass, VALUE size)
{
    char buffer[56];
    return rb_str_new2(sigar_format_size(NUM2LL(size), buffer));
}

static VALUE rb_sigar_net_address_to_string(sigar_net_address_t address)
{
    char addr_str[SIGAR_INET6_ADDRSTRLEN];
    sigar_net_address_to_string(NULL, &address, addr_str);
    return rb_str_new2(addr_str);
}

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

static VALUE rb_sigar_net_interface_list(VALUE obj)
{
    int status;
    sigar_t *sigar = rb_sigar_get(obj);
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

static VALUE rb_cSigarNetStat;

static VALUE rb_sigar_net_stat(VALUE obj)
{
    return obj; /*XXX*/
}

static VALUE rb_cSigarNetConnection;

static VALUE rb_sigar_net_connection(VALUE obj)
{
    return obj; /*XXX*/
}

static VALUE rb_cSigarCpuInfo;

static VALUE rb_sigar_cpu_info(VALUE obj)
{
    return obj; /*XXX*/
}

static VALUE rb_cSigarFileSystem;

static VALUE rb_sigar_file_system_list(VALUE obj)
{
    int status;
    sigar_t *sigar = rb_sigar_get(obj);
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

static VALUE rb_cSigarWho;

static VALUE rb_sigar_who(VALUE obj)
{
    return obj; /*XXX*/
}

static VALUE rb_cSigarNetRoute;

static VALUE rb_sigar_net_route(VALUE obj)
{
    return obj; /*XXX*/
}

#include "./rbsigar_generated.rx"

void Init_rbsigar(void)
{
    VALUE rclass = rb_define_class("Sigar", rb_cObject);

    rb_define_method(rclass, "file_system_list", rb_sigar_file_system_list, 0);
    rb_define_method(rclass, "net_interface_list", rb_sigar_net_interface_list, 0);

    rb_define_singleton_method(rclass, "new", rb_sigar_new, 0);
    rb_define_singleton_method(rclass, "format_size", rb_sigar_format_size, 1);

    /* generated */
    rb_sigar_define_module_methods(rclass);
}
