/*
 * Copyright (c) 2007 Hyperic, Inc.
 * Copyright (c) 2010-2011 VMware, Inc.
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

#include <Python.h>
#include "sigar.h"
#include "sigar_fileinfo.h"
#include "sigar_format.h"
#include "sigar_ptql.h"

#define PySigarString_FromNetAddr(a) pysigar_net_address_to_string(&a)

#define PySigarInt_FromChar(c) PyInt_FromLong((int)c)

#define PySigar_ParsePID \
    if (pysigar_parse_pid(sigar, args, &pid) != SIGAR_OK) return NULL

#define PySigar_ParseName \
    if (!PyArg_ParseTuple(args, "s", &name, &name_len)) return NULL

#define PySIGAR_OBJ ((PySigarObject *)self)

#define PySIGAR ((sigar_t *)PySIGAR_OBJ->ptr)

#define PySigar_new(t) PyType_GenericAlloc(&t, 0)

#define PySigar_TPFLAGS Py_TPFLAGS_DEFAULT

#define PySigar_AddType(name, t) \
    if (PyType_Ready(&t) == 0) { \
        Py_INCREF(&t); \
        PyModule_AddObject(module, name, (PyObject *)&t); \
    }

#define PySigar_Croak() PyErr_SetString(PyExc_ValueError, sigar_strerror(sigar, status))

typedef struct {
    PyObject_HEAD
    void *ptr;
} PySigarObject;

static PyTypeObject pysigar_PySigarType;

static void pysigar_free(PyObject *self)
{
    if (PySIGAR_OBJ->ptr) {
        if (self->ob_type == &pysigar_PySigarType) {
            sigar_close(PySIGAR);
        }
        else {
            free(PySIGAR_OBJ->ptr);
        }
        PySIGAR_OBJ->ptr = NULL;
    }

    self->ob_type->tp_free((PyObject *)self);
}

static int pysigar_ptql_re_impl(void *data,
                                char *haystack, char *needle)
{
    PyObject *name = PyString_FromString("sigar");
    PyObject *module = PyImport_Import(name);
    PyObject *match, *args, *source, *regex, *result;
    int matches = 0;

    match = PyObject_GetAttrString(module, "string_matches");

    source = PyString_FromString(haystack);
    regex = PyString_FromString(needle);
    args = PyTuple_New(2);
    PyTuple_SetItem(args, 0, source); /* steals source reference */
    PyTuple_SetItem(args, 1, regex); /* steals regex reference */

    result = PyObject_CallObject(match, args);

    Py_DECREF(name);
    Py_DECREF(module);
    Py_DECREF(args);
    Py_DECREF(match);

    if (result == NULL) {
        if (PyErr_Occurred()) {
            PyErr_Print();
        }
    }
    else {
        matches = (result == Py_True);
        Py_DECREF(result);
    }

    return matches;
}

#define sigar_isdigit(c) \
    (isdigit(((unsigned char)(c))))

static int pysigar_parse_pid(sigar_t *sigar, PyObject *args, long *pid)
{
    if ((PyTuple_Size(args) >= 1) && PyString_Check(PyTuple_GetItem(args, 0))) {
        char *ptql;
        int ptql_len;

        if (!PyArg_ParseTuple(args, "s#", &ptql, &ptql_len)) {
            return !SIGAR_OK;
        }

        if (sigar_isdigit(*ptql)) {
            /* XXX pluck strtoull define from sigar_ptql.c */
            PyObject *obj = PyLong_FromString(ptql, &ptql, 10);
            *pid = PyLong_AsLong(obj);
            Py_DECREF(obj);
            return SIGAR_OK;
        }
        else if ((ptql_len == 2) &&
                 (*ptql == '$') && (*(ptql + 1) == '$'))
        {
            *pid = sigar_pid_get(sigar);
            return SIGAR_OK;
        }
        else {
            /* XXX cache queries */
            sigar_ptql_query_t *query;
            sigar_ptql_error_t error;
            int status;

            status = sigar_ptql_query_create(&query, ptql, &error);
            if (status == SIGAR_OK) {
                sigar_ptql_re_impl_set(sigar, NULL, pysigar_ptql_re_impl);
                status = sigar_ptql_query_find_process(sigar, query, (sigar_pid_t *)pid);
                sigar_ptql_re_impl_set(sigar, NULL, NULL);
                sigar_ptql_query_destroy(query);

                if (status == SIGAR_OK) {
                    return SIGAR_OK;
                }
                else {
                    PySigar_Croak();
                }
            }
            else {
                PyErr_SetString(PyExc_ValueError, error.message);
                return !SIGAR_OK;
            }
        }
    }

    if (PyArg_ParseTuple(args, "i", pid)) {
        return SIGAR_OK;
    }
    else {
        return !SIGAR_OK;
    }
}

static PyObject *pysigar_net_address_to_string(sigar_net_address_t *address)
{
    char addr_str[SIGAR_INET6_ADDRSTRLEN];
    sigar_net_address_to_string(NULL, address, addr_str);
    return PyString_FromString(addr_str);
}

#include "_sigar_generated.c"

static PyObject *pysigar_open(PyObject *pyself, PyObject *args)
{
    PyObject *self = PySigar_new(pysigar_PySigarType);
    sigar_open((sigar_t **)&PySIGAR_OBJ->ptr);
    return self;
}

static PyObject *pysigar_close(PyObject *self, PyObject *args)
{
    if (PySIGAR_OBJ->ptr) {
        sigar_close(PySIGAR);
        PySIGAR_OBJ->ptr = NULL;
    }

    Py_INCREF(Py_None);
    return Py_None;
}

static int pysigar_parse_uint64(PyObject *args, sigar_uint64_t *val)
{
    PyObject *obj;

    if (!PyArg_ParseTuple(args, "O", &obj)) {
        return !SIGAR_OK;
    }

    if (PyInt_Check(obj)) {
        *val = PyInt_AsUnsignedLongLongMask(obj);
    }
    else if (PyLong_Check(obj)) {
        *val = PyLong_AsUnsignedLongLong(obj);
    }
    else {
        return !SIGAR_OK;
    }
    return SIGAR_OK;
}

static PyObject *pysigar_new_strlist(char **data, unsigned long number)
{
    unsigned long i;
    PyObject *av;

    if (!(av = PyTuple_New(number))) {
        return NULL;
    }

    for (i=0; i<number; i++) {
        PyTuple_SET_ITEM(av, i, PyString_FromString(data[i]));
    }

    return av;
}

static PyObject *pysigar_new_doublelist(double *data, unsigned long number)
{
    unsigned long i;
    PyObject *av;

    if (!(av = PyTuple_New(number))) {
        return NULL;
    }

    for (i=0; i<number; i++) {
        PyTuple_SET_ITEM(av, i, PyFloat_FromDouble(data[i]));
    }

    return av;
}

static PyObject *pysigar_new_intlist(int *data, unsigned long number)
{
    unsigned long i;
    PyObject *av;

    if (!(av = PyTuple_New(number))) {
        return NULL;
    }

    for (i=0; i<number; i++) {
        PyTuple_SET_ITEM(av, i, PyInt_FromLong(data[i]));
    }

    return av;
}

static PyObject *pysigar_new_list(char *data, unsigned long number,
                                  int size, PyTypeObject *type)
{
    unsigned long i;
    PyObject *av = PyTuple_New(number);

    for (i=0; i<number; i++, data += size) {
        void *ent = malloc(size);
        PyObject *self = PyType_GenericAlloc(type, 0);
        memcpy(ent, data, size);
        PySIGAR_OBJ->ptr = ent;
        PyTuple_SET_ITEM(av, i, self);
    }

    return av;
}

static PyObject *pysigar_file_system_list(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_file_system_list_t fslist;
    PyObject *RETVAL;

    status = sigar_file_system_list_get(sigar, &fslist);
    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_list((char *)&fslist.data[0],
                              fslist.number,
                              sizeof(*fslist.data),
                              &pysigar_PySigarFileSystemType);

    sigar_file_system_list_destroy(sigar, &fslist);

    return RETVAL;
}

static PyObject *pysigar_net_interface_list(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_net_interface_list_t iflist;
    PyObject *RETVAL;

    status = sigar_net_interface_list_get(sigar, &iflist);
    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_strlist(iflist.data, iflist.number);

    sigar_net_interface_list_destroy(sigar, &iflist);

    return RETVAL;
}

static PyObject *pysigar_net_connection_list(PyObject *self, PyObject *args)
{
    int status, flags;
    sigar_t *sigar = PySIGAR;
    sigar_net_connection_list_t connlist;
    PyObject *RETVAL;

    if (!PyArg_ParseTuple(args, "i", &flags)) {
        return NULL;
    }

    status = sigar_net_connection_list_get(sigar, &connlist, flags);

    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_list((char *)&connlist.data[0],
                              connlist.number,
                              sizeof(*connlist.data),
                              &pysigar_PySigarNetConnectionType);

    sigar_net_connection_list_destroy(sigar, &connlist);

    return RETVAL;
}

static PyObject *pysigar_net_route_list(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_net_route_list_t net_routelist;
    PyObject *RETVAL;

    status = sigar_net_route_list_get(sigar, &net_routelist);
    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_list((char *)&net_routelist.data[0],
                              net_routelist.number,
                              sizeof(*net_routelist.data),
                              &pysigar_PySigarNetRouteType);

    sigar_net_route_list_destroy(sigar, &net_routelist);

    return RETVAL;
}

static PyObject *pysigar_arp_list(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_arp_list_t arplist;
    PyObject *RETVAL;

    status = sigar_arp_list_get(sigar, &arplist);
    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_list((char *)&arplist.data[0],
                              arplist.number,
                              sizeof(*arplist.data),
                              &pysigar_PySigarArpType);

    sigar_arp_list_destroy(sigar, &arplist);

    return RETVAL;
}

static PyObject *pysigar_net_stat(PyObject *self, PyObject *args)
{
    int status, flags;
    sigar_t *sigar = PySIGAR;
    sigar_net_stat_t *RETVAL;

    if (!PyArg_ParseTuple(args, "i", &flags)) {
        return NULL;
    }

    RETVAL = malloc(sizeof(*RETVAL));
    if ((status = sigar_net_stat_get(sigar, RETVAL, flags)) != SIGAR_OK) {
        free(RETVAL);
        PySigar_Croak();
        return NULL;
    }
    else {
        PyObject *self = PySigar_new(pysigar_PySigarNetStatType);
        PySIGAR_OBJ->ptr = RETVAL;
        return self;
    }
}

static PyObject *pysigar_cpu_list(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_cpu_list_t cpus;
    PyObject *RETVAL;

    status = sigar_cpu_list_get(sigar, &cpus);
    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_list((char *)&cpus.data[0],
                              cpus.number,
                              sizeof(*cpus.data),
                              &pysigar_PySigarCpuType);

    sigar_cpu_list_destroy(sigar, &cpus);

    return RETVAL;
}

static PyObject *pysigar_cpu_info_list(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_cpu_info_list_t cpu_infos;
    PyObject *RETVAL;

    status = sigar_cpu_info_list_get(sigar, &cpu_infos);
    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_list((char *)&cpu_infos.data[0],
                              cpu_infos.number,
                              sizeof(*cpu_infos.data),
                              &pysigar_PySigarCpuInfoType);

    sigar_cpu_info_list_destroy(sigar, &cpu_infos);

    return RETVAL;
}

static PyObject *pysigar_loadavg(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_loadavg_t loadavg;

    status = sigar_loadavg_get(sigar, &loadavg);
    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    return pysigar_new_doublelist(loadavg.loadavg, 3);
}

static PyObject *pysigar_who_list(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_who_list_t wholist;
    PyObject *RETVAL;

    status = sigar_who_list_get(sigar, &wholist);
    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_list((char *)&wholist.data[0],
                              wholist.number,
                              sizeof(*wholist.data),
                              &pysigar_PySigarWhoType);

    sigar_who_list_destroy(sigar, &wholist);

    return RETVAL;
}

static PyObject *pysigar_proc_list(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_proc_list_t list;

    PyObject *RETVAL;

    if (PyTuple_Size(args) == 0) {
        status = sigar_proc_list_get(sigar, &list);

        if (status != SIGAR_OK) {
            PySigar_Croak();
            return NULL;
        }
    }
    else {
        sigar_ptql_query_t *query;
        sigar_ptql_error_t error;
        char *ptql;

        if (!PyArg_ParseTuple(args, "s", &ptql)) {
            return NULL;
        }

        status = sigar_ptql_query_create(&query, ptql, &error);

        if (status != SIGAR_OK) {
            PyErr_SetString(PyExc_ValueError, error.message);
            return NULL;
        }
        sigar_ptql_re_impl_set(sigar, NULL, pysigar_ptql_re_impl);
        status = sigar_ptql_query_find(sigar, query, &list);
        sigar_ptql_re_impl_set(sigar, NULL, NULL);
        sigar_ptql_query_destroy(query);
        if (status != SIGAR_OK) {
            PySigar_Croak();
            return NULL;
        }
    }

    RETVAL = pysigar_new_intlist(&list.data[0], list.number);

    sigar_proc_list_destroy(sigar, &list);

    return RETVAL;
}

static PyObject *pysigar_proc_args(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_proc_args_t proc_args;
    long pid;

    PyObject *RETVAL;

    PySigar_ParsePID;

    status = sigar_proc_args_get(sigar, pid, &proc_args);

    if (status != SIGAR_OK) {
        PySigar_Croak();
        return NULL;
    }

    RETVAL = pysigar_new_strlist(proc_args.data, proc_args.number);

    sigar_proc_args_destroy(sigar, &proc_args);

    return RETVAL;
}

static int pysigar_env_getall(void *data,
                              const char *key, int klen,
                              char *val, int vlen)
{
    PyDict_SetItem((PyObject *)data,
                   PyString_FromStringAndSize(key, klen),
                   PyString_FromStringAndSize(val, vlen));

    return SIGAR_OK;
}

static PyObject *pysigar_proc_env(PyObject *self, PyObject *args)
{
    int status;
    sigar_t *sigar = PySIGAR;
    sigar_proc_env_t procenv;
    long pid;

    PyObject *RETVAL;

    PySigar_ParsePID;

    RETVAL = PyDict_New();

    procenv.type = SIGAR_PROC_ENV_ALL;
    procenv.env_getter = pysigar_env_getall;
    procenv.data = RETVAL;

    status = sigar_proc_env_get(sigar, pid, &procenv);
    if (status != SIGAR_OK) {
        PySigar_Croak();
    }

    return RETVAL;
}

static PyObject *pysigar_format_size(PyObject *self, PyObject *args)
{
    char buffer[56];
    sigar_uint64_t size;

    if (pysigar_parse_uint64(args, &size) == SIGAR_OK) {
        return PyString_FromString(sigar_format_size(size, buffer));
    }
    else {
        return NULL;
    }
}

static PyMethodDef pysigar_methods[] = {
    { "close", pysigar_close, METH_NOARGS, NULL },
    { "net_interface_list", pysigar_net_interface_list, METH_NOARGS, NULL },
    { "net_connection_list", pysigar_net_connection_list, METH_VARARGS, NULL },
    { "net_route_list", pysigar_net_route_list, METH_NOARGS, NULL },
    { "file_system_list", pysigar_file_system_list, METH_NOARGS, NULL },
    { "arp_list", pysigar_arp_list, METH_NOARGS, NULL },
    { "net_stat", pysigar_net_stat, METH_VARARGS, NULL },
    { "cpu_list", pysigar_cpu_list, METH_NOARGS, NULL },
    { "cpu_info_list", pysigar_cpu_info_list, METH_NOARGS, NULL },
    { "loadavg", pysigar_loadavg, METH_NOARGS, NULL },
    { "who_list", pysigar_who_list, METH_NOARGS, NULL },
    { "proc_list", pysigar_proc_list, METH_VARARGS, NULL },
    { "proc_args", pysigar_proc_args, METH_VARARGS, NULL },
    { "proc_env", pysigar_proc_env, METH_VARARGS, NULL },
    PY_SIGAR_METHODS
    {NULL}
};

static PyTypeObject pysigar_PySigarType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /*ob_size*/
    "Sigar",                   /*tp_name*/
    sizeof(PySigarObject),     /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    pysigar_free,              /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0,                         /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    PySigar_TPFLAGS,           /*tp_flags*/
    0,                         /*tp_doc*/
    0,                         /*tp_traverse*/
    0,                         /*tp_clear*/
    0,                         /*tp_richcompare*/
    0,                         /*tp_weaklistoffset*/
    0,                         /*tp_iter*/
    0,                         /*tp_iternext*/
    pysigar_methods,           /*tp_methods*/
    0,                         /*tp_members*/
    0,                         /*tp_getset*/
    0,                         /*tp_base*/
    0,                         /*tp_dict*/
    0,                         /*tp_descr_get*/
    0,                         /*tp_descr_set*/
    0,                         /*tp_dictoffset*/
    0,                         /*tp_init*/
    0,                         /*tp_alloc*/
    0                          /*tp_new*/
};

static PyMethodDef pysigar_module_methods[] = {
    { "open", pysigar_open, METH_NOARGS, NULL },
    { "format_size", pysigar_format_size, METH_VARARGS, NULL },
    {NULL}
};

#define PY_SIGAR_CONST_INT(name) \
    PyDict_SetItemString(dict, #name, o=PyInt_FromLong(SIGAR_##name)); Py_DECREF(o)

#define PY_SIGAR_CONST_STR(name) \
    PyDict_SetItemString(dict, #name, o=PyString_FromString(SIGAR_##name)); Py_DECREF(o)

#define PY_SIGAR_DEFINE_CONST_STR(name, value) \
    PyDict_SetItemString(dict, name, o=PyString_FromString(value)); Py_DECREF(o)

static void init_pysigar_version(PyObject *dict)
{
    PyObject *o;
    sigar_version_t *sv = sigar_version_get();
    PY_SIGAR_DEFINE_CONST_STR("BUILD_DATE", sv->build_date);
    PY_SIGAR_DEFINE_CONST_STR("SCM_REVISION", sv->scm_revision);
    PY_SIGAR_DEFINE_CONST_STR("VERSION", sv->version);
}

static void init_pysigar_constants(PyObject *dict)
{
    PyObject *o;

    init_pysigar_version(dict);

    PY_SIGAR_CONST_INT(FIELD_NOTIMPL);

    PY_SIGAR_CONST_INT(IFF_UP);
    PY_SIGAR_CONST_INT(IFF_BROADCAST);
    PY_SIGAR_CONST_INT(IFF_DEBUG);
    PY_SIGAR_CONST_INT(IFF_LOOPBACK);
    PY_SIGAR_CONST_INT(IFF_POINTOPOINT);
    PY_SIGAR_CONST_INT(IFF_NOTRAILERS);
    PY_SIGAR_CONST_INT(IFF_RUNNING);
    PY_SIGAR_CONST_INT(IFF_NOARP);
    PY_SIGAR_CONST_INT(IFF_PROMISC);
    PY_SIGAR_CONST_INT(IFF_ALLMULTI);
    PY_SIGAR_CONST_INT(IFF_MULTICAST);

    PY_SIGAR_CONST_STR(NULL_HWADDR);
}

PyMODINIT_FUNC
init_sigar(void) 
{
    PyObject *module =
        Py_InitModule("_sigar", pysigar_module_methods);

    PySigar_AddType("Sigar", pysigar_PySigarType);

    PY_SIGAR_ADD_TYPES;

    init_pysigar_constants(PyModule_GetDict(module));
}
