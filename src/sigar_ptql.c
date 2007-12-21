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
#include "sigar_ptql.h"
#include "sigar_os.h"

#include <stdio.h>

#ifdef SIGAR_HAS_PCRE
#include "pcre.h"
#endif

/* See http://gcc.gnu.org/ml/libstdc++/2002-03/msg00164.html */
#if defined(WIN32) || (defined(__hpux) && defined(SIGAR_64BIT))
#define strtoull strtoul
#elif (defined(__hpux) && !defined(SIGAR_64BIT))
#define strtoull __strtoull
#else
#include <errno.h>
#endif

#define strtonum_failed(src, ptr) \
    ((src == ptr) || (errno == ERANGE) || (*ptr != '\0'))

typedef struct ptql_parse_branch_t ptql_parse_branch_t;
typedef struct ptql_branch_t ptql_branch_t;

/* adhere to calling convention, else risk stack corruption */
#ifdef WIN32
#define SIGAPI WINAPI
#else
#define SIGAPI
#endif

typedef int (SIGAPI *ptql_get_t)(sigar_t *sigar, sigar_pid_t pid, void *data);
typedef int (*ptql_branch_init_t)(ptql_parse_branch_t *parsed, ptql_branch_t *branch,
                                  sigar_ptql_error_t *error);

typedef int (*ptql_op_ui64_t)(ptql_branch_t *branch,
                              sigar_uint64_t haystack,
                              sigar_uint64_t needle);

typedef int (*ptql_op_ui32_t)(ptql_branch_t *branch,
                              sigar_uint32_t haystack,
                              sigar_uint32_t needle);

typedef int (*ptql_op_dbl_t)(ptql_branch_t *branch,
                             double haystack,
                             double needle);

typedef int (*ptql_op_str_t)(ptql_branch_t *branch,
                             char *haystack,
                             char *needle);

typedef int (*ptql_op_chr_t)(ptql_branch_t *branch,
                             char haystack,
                             char needle);

typedef enum {
    PTQL_VALUE_TYPE_UI64,
    PTQL_VALUE_TYPE_UI32,
    PTQL_VALUE_TYPE_DBL,
    PTQL_VALUE_TYPE_CHR,
    PTQL_VALUE_TYPE_STR,
    PTQL_VALUE_TYPE_ANY
} ptql_value_type_t;

typedef enum {
    PTQL_OP_EQ,
    PTQL_OP_NE,
    PTQL_OP_GT,
    PTQL_OP_GE,
    PTQL_OP_LT,
    PTQL_OP_LE,
#define PTQL_OP_MAX_NSTR PTQL_OP_LE
    PTQL_OP_EW, /* rest are string only */
    PTQL_OP_SW,
    PTQL_OP_RE,
    PTQL_OP_CT,
    PTQL_OP_MAX
} ptql_op_name_t;

#define PTQL_OP_FLAG_PARENT 1
#define PTQL_OP_FLAG_REF    2
#define PTQL_OP_FLAG_GLOB   4
#define PTQL_OP_FLAG_PID    8

struct ptql_parse_branch_t {
    char *name;
    char *attr;
    char *op;
    char *value;
    unsigned int op_flags;
};

typedef struct {
    char *name;
    ptql_get_t get;
    size_t offset;
    unsigned int data_size;
    ptql_value_type_t type;
    ptql_branch_init_t init;
} ptql_lookup_t;

#define DATA_PTR(branch) \
    ((char *)branch->data.ptr + branch->lookup->offset)

static void data_free(void *data)
{
    free(data);
}

typedef union {
    sigar_pid_t pid;
    sigar_uint64_t ui64;
    sigar_uint32_t ui32;
    double dbl;
    char chr[4];
    char *str;
    void *ptr;
} any_value_t;

struct ptql_branch_t {
    ptql_lookup_t *lookup;
    any_value_t data;
    unsigned int data_size;
    void (*data_free)(void *);
    unsigned int flags;
    unsigned int op_flags;
    ptql_op_name_t op_name;
    union {
        ptql_op_ui64_t ui64;
        ptql_op_ui32_t ui32;
        ptql_op_dbl_t dbl;
        ptql_op_chr_t chr;
        ptql_op_str_t str;
    } match;
    any_value_t value;
    void (*value_free)(void *);
};

typedef struct {
    char *name;
    ptql_lookup_t *members;
} ptql_entry_t;

typedef struct {
    unsigned long number;
    unsigned long size;
    ptql_branch_t *data;
} ptql_branch_list_t;

struct sigar_ptql_query_t {
    ptql_branch_list_t branches;
#ifdef PTQL_DEBUG
    char *ptql;
#endif
};

/* XXX optimize */
static ptql_op_name_t ptql_op_code_get(char *op)
{
    if (strEQ(op, "eq")) {
        return PTQL_OP_EQ;
    }
    else if (strEQ(op, "ne")) {
        return PTQL_OP_NE;
    }
    else if (strEQ(op, "gt")) {
        return PTQL_OP_GT;
    }
    else if (strEQ(op, "ge")) {
        return PTQL_OP_GE;
    }
    else if (strEQ(op, "lt")) {
        return PTQL_OP_LT;
    }
    else if (strEQ(op, "le")) {
        return PTQL_OP_LE;
    }
    else if (strEQ(op, "ew")) {
        return PTQL_OP_EW;
    }
    else if (strEQ(op, "sw")) {
        return PTQL_OP_SW;
    }
    else if (strEQ(op, "re")) {
        return PTQL_OP_RE;
    }
    else if (strEQ(op, "ct")) {
        return PTQL_OP_CT;
    }
    else {
        return PTQL_OP_MAX;
    }
}

static int ptql_op_ui64_eq(ptql_branch_t *branch,
                           sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack == needle;
}

static int ptql_op_ui64_ne(ptql_branch_t *branch,
                           sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack != needle;
}

static int ptql_op_ui64_gt(ptql_branch_t *branch,
                           sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack > needle;
}

static int ptql_op_ui64_ge(ptql_branch_t *branch,
                           sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack >= needle;
}

static int ptql_op_ui64_lt(ptql_branch_t *branch,
                           sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack < needle;
}

static int ptql_op_ui64_le(ptql_branch_t *branch,
                           sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack <= needle;
}

static ptql_op_ui64_t ptql_op_ui64[] = {
    ptql_op_ui64_eq,
    ptql_op_ui64_ne,
    ptql_op_ui64_gt,
    ptql_op_ui64_ge,
    ptql_op_ui64_lt,
    ptql_op_ui64_le
};

static int ptql_op_ui32_eq(ptql_branch_t *branch,
                           sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack == needle;
}

static int ptql_op_ui32_ne(ptql_branch_t *branch,
                           sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack != needle;
}

static int ptql_op_ui32_gt(ptql_branch_t *branch,
                           sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack > needle;
}

static int ptql_op_ui32_ge(ptql_branch_t *branch,
                           sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack >= needle;
}

static int ptql_op_ui32_lt(ptql_branch_t *branch,
                           sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack < needle;
}

static int ptql_op_ui32_le(ptql_branch_t *branch,
                           sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack <= needle;
}

static ptql_op_ui32_t ptql_op_ui32[] = {
    ptql_op_ui32_eq,
    ptql_op_ui32_ne,
    ptql_op_ui32_gt,
    ptql_op_ui32_ge,
    ptql_op_ui32_lt,
    ptql_op_ui32_le
};

static int ptql_op_dbl_eq(ptql_branch_t *branch,
                          double haystack, double needle)
{
    return haystack == needle;
}

static int ptql_op_dbl_ne(ptql_branch_t *branch,
                          double haystack, double needle)
{
    return haystack != needle;
}

static int ptql_op_dbl_gt(ptql_branch_t *branch,
                          double haystack, double needle)
{
    return haystack > needle;
}

static int ptql_op_dbl_ge(ptql_branch_t *branch,
                          double haystack, double needle)
{
    return haystack >= needle;
}

static int ptql_op_dbl_lt(ptql_branch_t *branch,
                          double haystack, double needle)
{
    return haystack < needle;
}

static int ptql_op_dbl_le(ptql_branch_t *branch,
                          double haystack, double needle)
{
    return haystack <= needle;
}

static ptql_op_dbl_t ptql_op_dbl[] = {
    ptql_op_dbl_eq,
    ptql_op_dbl_ne,
    ptql_op_dbl_gt,
    ptql_op_dbl_ge,
    ptql_op_dbl_lt,
    ptql_op_dbl_le
};

static int ptql_op_str_eq(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    return strEQ(haystack, needle);
}

static int ptql_op_str_ne(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    return !strEQ(haystack, needle);
}

static int ptql_op_str_gt(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    return strcmp(haystack, needle) > 0;
}

static int ptql_op_str_ge(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    return strcmp(haystack, needle) >= 0;
}

static int ptql_op_str_lt(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    return strcmp(haystack, needle) < 0;
}

static int ptql_op_str_le(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    return strcmp(haystack, needle) <= 0;
}

static int ptql_op_str_ew(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    int nlen = strlen(needle);
    int hlen = strlen(haystack);
    int diff = hlen - nlen;
    if (diff < 0) {
        return 0;
    }
    return strnEQ(haystack + diff, needle, nlen);
}

static int ptql_op_str_sw(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    return strnEQ(haystack, needle, strlen(needle));
}

static int ptql_op_str_re(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
#ifdef SIGAR_HAS_PCRE
    pcre *re = (pcre *)branch->value.ptr;
    int len = strlen(haystack);
    int rc =
        pcre_exec(re, NULL, haystack, len, 0, 0, NULL, 0);
    return rc >= 0;
#else
    return 0;
#endif
}

static int ptql_op_str_ct(ptql_branch_t *branch,
                          char *haystack, char *needle)
{
    return strstr(haystack, needle) != NULL;
}

static ptql_op_str_t ptql_op_str[] = {
    ptql_op_str_eq,
    ptql_op_str_ne,
    ptql_op_str_gt,
    ptql_op_str_ge,
    ptql_op_str_lt,
    ptql_op_str_le,
    ptql_op_str_ew,
    ptql_op_str_sw,
    ptql_op_str_re,
    ptql_op_str_ct
};

static int ptql_op_chr_eq(ptql_branch_t *branch,
                          char haystack, char needle)
{
    return haystack == needle;
}

static int ptql_op_chr_ne(ptql_branch_t *branch,
                          char haystack, char needle)
{
    return haystack != needle;
}

static int ptql_op_chr_gt(ptql_branch_t *branch,
                          char haystack, char needle)
{
    return haystack > needle;
}

static int ptql_op_chr_ge(ptql_branch_t *branch,
                          char haystack, char needle)
{
    return haystack >= needle;
}

static int ptql_op_chr_lt(ptql_branch_t *branch,
                          char haystack, char needle)
{
    return haystack < needle;
}

static int ptql_op_chr_le(ptql_branch_t *branch,
                          char haystack, char needle)
{
    return haystack <= needle;
}

static ptql_op_chr_t ptql_op_chr[] = {
    ptql_op_chr_eq,
    ptql_op_chr_ne,
    ptql_op_chr_gt,
    ptql_op_chr_ge,
    ptql_op_chr_lt,
    ptql_op_chr_le
};

#define PTQL_BRANCH_LIST_MAX 3

#define PTQL_BRANCH_LIST_GROW(branches) \
    if ((branches)->number >= (branches)->size) { \
        ptql_branch_list_grow(branches); \
    }

static int ptql_branch_list_create(ptql_branch_list_t *branches)
{
    branches->number = 0;
    branches->size = PTQL_BRANCH_LIST_MAX;
    branches->data = malloc(sizeof(*(branches->data)) *
                            branches->size);

    return SIGAR_OK;
}

static int ptql_branch_list_grow(ptql_branch_list_t *branches)
{
    branches->data =
        realloc(branches->data,
                sizeof(*(branches->data)) *
                (branches->size + PTQL_BRANCH_LIST_MAX));
    branches->size += PTQL_BRANCH_LIST_MAX;

    return SIGAR_OK;
}

static int ptql_branch_list_destroy(ptql_branch_list_t *branches)
{
    if (branches->size) {
        int i;

        for (i=0; i<branches->number; i++) {
            ptql_branch_t *branch =
                &branches->data[i];

            if (branch->data_size && branch->data.ptr) {
                branch->data_free(branch->data.ptr);
            }

            if (branch->lookup &&
                (branch->lookup->type == PTQL_VALUE_TYPE_STR) &&
                !(branch->op_flags & PTQL_OP_FLAG_REF))
            {
                if (branch->value.str) {
                    branch->value_free(branch->value.str);
                }
            }
        }

        free(branches->data);
        branches->number = branches->size = 0;
    }

    return SIGAR_OK;
}

#ifdef WIN32
#define vsnprintf _vsnprintf
#endif

#define PTQL_ERRNAN \
    ptql_error(error, "Query value '%s' is not a number", parsed->value)

static int ptql_error(sigar_ptql_error_t *error, const char *format, ...)
{
    va_list args;

    if (error != NULL) {
        va_start(args, format);
        vsnprintf(error->message, sizeof(error->message), format, args);
        va_end(args);
    }

    return SIGAR_PTQL_MALFORMED_QUERY;
}

static int ptql_branch_init_any(ptql_parse_branch_t *parsed,
                                ptql_branch_t *branch,
                                sigar_ptql_error_t *error)
{
    branch->data.str = sigar_strdup(parsed->attr);
    branch->data_size = strlen(parsed->attr);
    return SIGAR_OK;
}

static int ptql_str_match(sigar_t *sigar, ptql_branch_t *branch, char *value)
{
    if (!branch->value.str) {
        return 0;
    }
#ifndef SIGAR_HAS_PCRE
    if (branch->op_name == PTQL_OP_RE) {
        if (sigar->ptql_re_impl) {
            return sigar->ptql_re_impl(sigar->ptql_re_data,
                                       value,
                                       branch->value.str);
        }
        else {
            return 0;
        }
    }
#endif
    return branch->match.str(branch,
                             value,
                             branch->value.str);
}

static int ptql_branch_match(ptql_branch_t *branch)
{
    switch (branch->lookup->type) {
      case PTQL_VALUE_TYPE_UI64:
        return branch->match.ui64(branch,
                                  *(sigar_uint64_t *)DATA_PTR(branch),
                                  branch->value.ui64);
      case PTQL_VALUE_TYPE_UI32:
        return branch->match.ui32(branch,
                                  *(sigar_uint32_t *)DATA_PTR(branch),
                                  branch->value.ui32);
      case PTQL_VALUE_TYPE_CHR:
        return branch->match.chr(branch,
                                 *(char *)DATA_PTR(branch),
                                 branch->value.chr[0]);
      case PTQL_VALUE_TYPE_STR:
      case PTQL_VALUE_TYPE_ANY:
        if (!branch->value.str) {
            return 0;
        }
        return branch->match.str(branch,
                                 (char *)DATA_PTR(branch),
                                 branch->value.str);
      default:
        return 0;
    }
}

static int ptql_branch_match_ref(ptql_branch_t *branch, ptql_branch_t *ref)
{
    switch (branch->lookup->type) {
      case PTQL_VALUE_TYPE_UI64:
        return branch->match.ui64(branch,
                                  *(sigar_uint64_t *)DATA_PTR(branch),
                                  *(sigar_uint64_t *)DATA_PTR(ref));
      case PTQL_VALUE_TYPE_UI32:
        return branch->match.ui32(branch,
                                  *(sigar_uint32_t *)DATA_PTR(branch),
                                  *(sigar_uint32_t *)DATA_PTR(ref));
      case PTQL_VALUE_TYPE_CHR:
        return branch->match.chr(branch,
                                 *(char *)DATA_PTR(branch),
                                 *(char *)DATA_PTR(ref));
      case PTQL_VALUE_TYPE_STR:
      case PTQL_VALUE_TYPE_ANY:
        return branch->match.str(branch,
                                 (char *)DATA_PTR(branch),
                                 (char *)DATA_PTR(ref));
      default:
        return 0;
    }
}

enum {
    PTQL_PID_PID,
    PTQL_PID_FILE,
    PTQL_PID_SERVICE
};

#ifdef SIGAR_64BIT

#define str2pid(value, ptr) strtoull(value, &ptr, 10)

#define pid_branch_match(branch, pid, match_pid) \
    ptql_op_ui64[branch->op_name](branch, pid, match_pid)

#else

#define str2pid(value, ptr) strtoul(value, &ptr, 10)

#define pid_branch_match(branch, pid, match_pid) \
    ptql_op_ui32[branch->op_name](branch, pid, match_pid)

#endif

static int ptql_branch_init_pid(ptql_parse_branch_t *parsed,
                                ptql_branch_t *branch,
                                sigar_ptql_error_t *error)
{
    branch->op_flags |= PTQL_OP_FLAG_PID;

    if (strEQ(parsed->attr, "Pid")) {
        branch->flags = PTQL_PID_PID;
        if (strEQ(parsed->value, "$$")) {
            branch->data.pid = getpid();
        }
        else {
            char *ptr;
            branch->data.pid = str2pid(parsed->value, ptr);
            if (strtonum_failed(parsed->value, ptr)) {
                return PTQL_ERRNAN;
            }
        }
        return SIGAR_OK;
    }
    else if (strEQ(parsed->attr, "PidFile")) {
        branch->flags = PTQL_PID_FILE;
        branch->data.str = sigar_strdup(parsed->value);
        branch->data_size = strlen(parsed->value);
        return SIGAR_OK;
    }
    else if (strEQ(parsed->attr, "Service")) {
        branch->flags = PTQL_PID_SERVICE;
#ifdef WIN32
        branch->data.str = sigar_strdup(parsed->value);
        branch->data_size = strlen(parsed->value);
#endif
        return SIGAR_OK;
    }
    else {
        return ptql_error(error, "Unsupported %s attribute: %s",
                          parsed->name, parsed->attr);
    }
}

static int ptql_pid_get(sigar_t *sigar,
                        ptql_branch_t *branch,
                        sigar_pid_t *pid)
{
    char *ptr;

    if (branch->flags == PTQL_PID_FILE) {
        char buffer[SIGAR_PATH_MAX+1];
        int status =
            sigar_file2str((const char *)branch->data.str,
                           buffer, sizeof(buffer)-1);
        if (status != SIGAR_OK) {
            return status;
        }
        *pid = str2pid(buffer, ptr);
        if ((buffer == ptr) || (errno == ERANGE)) {
            return errno;
        }
    }
    else if (branch->flags == PTQL_PID_SERVICE) {
#ifdef WIN32
        int status =
            sigar_service_pid_get(sigar,
                                  branch->data.str, pid);
        if (status != SIGAR_OK) {
            return status;
        }
#else
        return SIGAR_ENOTIMPL;
#endif
    }
    else {
        *pid = branch->data.pid;
    }

    return SIGAR_OK;
}

static int SIGAPI ptql_pid_match(sigar_t *sigar,
                                 sigar_pid_t pid,
                                 void *data)
{
    ptql_branch_t *branch =
        (ptql_branch_t *)data;
    sigar_pid_t match_pid;
    int matched;
    int status =
        ptql_pid_get(sigar, branch, &match_pid);

    if (status != SIGAR_OK) {
        return status;
    }

    matched = pid_branch_match(branch, pid, match_pid);

    return matched ? SIGAR_OK : 1;
}

static int ptql_args_branch_init(ptql_parse_branch_t *parsed,
                                 ptql_branch_t *branch,
                                 sigar_ptql_error_t *error)
{
    if (strEQ(parsed->attr, "*")) {
        branch->op_flags |= PTQL_OP_FLAG_GLOB;
    }
    else {
        char *end;

        branch->data.ui32 =
            strtol(parsed->attr, &end, 10);

        if (strtonum_failed(parsed->attr, end)) {
            /* conversion failed */
            return ptql_error(error, "%s is not a number", parsed->attr);
        }
    }
    return SIGAR_OK;
}

static int SIGAPI ptql_args_match(sigar_t *sigar,
                                  sigar_pid_t pid,
                                  void *data)
{
    ptql_branch_t *branch =
        (ptql_branch_t *)data;
    int status, matched=0;
    sigar_proc_args_t args;

    status = sigar_proc_args_get(sigar, pid, &args);
    if (status != SIGAR_OK) {
        return status;
    }

    if (branch->op_flags & PTQL_OP_FLAG_GLOB) {
        int i;
        for (i=0; i<args.number; i++) {
            matched = 
                ptql_str_match(sigar, branch, args.data[i]);

            if (matched) {
                break;
            }
        }
    }
    else {
        int num = branch->data.ui32;

        /* e.g. find last element of args: Args.-1.eq=weblogic.Server */
        if (num < 0) {
            num += args.number;
        }
        if ((num >= 0) && (num < args.number)) {
            matched =
                ptql_str_match(sigar, branch, args.data[num]);
        }
    }

    sigar_proc_args_destroy(sigar, &args);

    return matched ? SIGAR_OK : !SIGAR_OK;
}

typedef struct {
    const char *key;
    int klen;
    char *val;
    int vlen;
} sigar_proc_env_entry_t;

static int sigar_proc_env_get_key(void *data,
                                  const char *key, int klen,
                                  char *val, int vlen)
{
    sigar_proc_env_entry_t *entry =
        (sigar_proc_env_entry_t *)data;

    if ((entry->klen == klen) &&
        (strcmp(entry->key, key) == 0))
    {
        entry->val = val;
        entry->vlen = vlen;
        return !SIGAR_OK; /* foundit; stop iterating */
    }

    return SIGAR_OK;
}

static int SIGAPI ptql_env_match(sigar_t *sigar,
                                 sigar_pid_t pid,
                                 void *data)
{
    ptql_branch_t *branch =
        (ptql_branch_t *)data;
    int status, matched=0;
    sigar_proc_env_t procenv;
    sigar_proc_env_entry_t entry;

    /* XXX ugh this is klunky */
    entry.key = branch->data.str;
    entry.klen = branch->data_size;
    entry.val = NULL;

    procenv.type = SIGAR_PROC_ENV_KEY;
    procenv.key  = branch->data.str;
    procenv.klen = branch->data_size;
    procenv.env_getter = sigar_proc_env_get_key;
    procenv.data = &entry;

    status = sigar_proc_env_get(sigar, pid, &procenv);
    if (status != SIGAR_OK) { 
        return status;
    }
    else {
        if (entry.val) {
            matched = 
                ptql_str_match(sigar, branch, entry.val);
        }
    }

    return matched ? SIGAR_OK : !SIGAR_OK;
}

static int ptql_branch_init_port(ptql_parse_branch_t *parsed,
                                 ptql_branch_t *branch,
                                 sigar_ptql_error_t *error)
{
    char *ptr;

    /* only 'eq' is supported here */
    if (branch->op_name != PTQL_OP_EQ) {
        return ptql_error(error, "%s requires 'eq' operator",
                          parsed->name);
    }

    if (strEQ(parsed->attr, "tcp")) {
        branch->flags = SIGAR_NETCONN_TCP;
    }
    else if (strEQ(parsed->attr, "udp")) {
        branch->flags = SIGAR_NETCONN_UDP;
    }
    else {
        return ptql_error(error, "Unsupported %s protocol: %s",
                             parsed->name, parsed->attr);
    }

    branch->data.ui32 = strtoul(parsed->value, &ptr, 10);
    if (strtonum_failed(parsed->value, ptr)) {
        return PTQL_ERRNAN;
    }

    return SIGAR_OK;
}

static int SIGAPI ptql_port_match(sigar_t *sigar,
                                  sigar_pid_t pid,
                                  void *data)
{
    ptql_branch_t *branch =
        (ptql_branch_t *)data;
    unsigned long port =
        branch->data.ui32;
    int status;
    sigar_pid_t match_pid=0;

    status =
        sigar_proc_port_get(sigar, branch->flags, port, &match_pid);

    if (status != SIGAR_OK) {
        return status;
    }

    return (pid == match_pid) ? SIGAR_OK : !SIGAR_OK;
}

#define PTQL_LOOKUP_ENTRY(cname, member, type) \
    (ptql_get_t)sigar_##cname##_get, \
    sigar_offsetof(sigar_##cname##_t, member), \
    sizeof(sigar_##cname##_t), \
    PTQL_VALUE_TYPE_##type, \
    NULL

/* XXX uid/pid can be larger w/ 64bit mode */
#define PTQL_VALUE_TYPE_PID PTQL_VALUE_TYPE_UI32
#define PTQL_VALUE_TYPE_UID PTQL_VALUE_TYPE_UI32

static ptql_lookup_t PTQL_Time[] = {
    { "StartTime", PTQL_LOOKUP_ENTRY(proc_time, start_time, UI64) },
    { "User",      PTQL_LOOKUP_ENTRY(proc_time, user, UI64) },
    { "Sys",       PTQL_LOOKUP_ENTRY(proc_time, sys, UI64) },
    { "Total",     PTQL_LOOKUP_ENTRY(proc_time, total, UI64) },
    { NULL }
};

static ptql_lookup_t PTQL_Cpu[] = {
    { "StartTime", PTQL_LOOKUP_ENTRY(proc_cpu, start_time, UI64) },
    { "User",      PTQL_LOOKUP_ENTRY(proc_cpu, user, UI64) },
    { "Sys",       PTQL_LOOKUP_ENTRY(proc_cpu, sys, UI64) },
    { "Total",     PTQL_LOOKUP_ENTRY(proc_cpu, total, UI64) },
    { "Percent",   PTQL_LOOKUP_ENTRY(proc_cpu, total, DBL) },
    { NULL }
};

static ptql_lookup_t PTQL_CredName[] = {
    { "User",  PTQL_LOOKUP_ENTRY(proc_cred_name, user, STR) },
    { "Group", PTQL_LOOKUP_ENTRY(proc_cred_name, group, STR) },
    { NULL }
};

static ptql_lookup_t PTQL_Mem[] = {
    { "Size",        PTQL_LOOKUP_ENTRY(proc_mem, size, UI64) },
    { "Resident",    PTQL_LOOKUP_ENTRY(proc_mem, resident, UI64) },
    { "Share",       PTQL_LOOKUP_ENTRY(proc_mem, share, UI64) },
    { "MinorFaults", PTQL_LOOKUP_ENTRY(proc_mem, minor_faults, UI64) },
    { "MajorFaults", PTQL_LOOKUP_ENTRY(proc_mem, major_faults, UI64) },
    { "PageFaults",  PTQL_LOOKUP_ENTRY(proc_mem, page_faults, UI64) },
    { NULL }
};

static ptql_lookup_t PTQL_Exe[] = {
    { "Name", PTQL_LOOKUP_ENTRY(proc_exe, name, STR) },
    { "Cwd",  PTQL_LOOKUP_ENTRY(proc_exe, cwd, STR) },
    { NULL }
};

static ptql_lookup_t PTQL_Cred[] = {
    { "Uid",  PTQL_LOOKUP_ENTRY(proc_cred, uid, UID) },
    { "Gid",  PTQL_LOOKUP_ENTRY(proc_cred, gid, UID) },
    { "Euid", PTQL_LOOKUP_ENTRY(proc_cred, euid, UID) },
    { "Egid", PTQL_LOOKUP_ENTRY(proc_cred, egid, UID) },
    { NULL }
};

static ptql_lookup_t PTQL_State[] = {
    { "State",     PTQL_LOOKUP_ENTRY(proc_state, state, CHR) },
    { "Name",      PTQL_LOOKUP_ENTRY(proc_state, name, STR) },
    { "Ppid",      PTQL_LOOKUP_ENTRY(proc_state, ppid, PID) },
    { "Tty",       PTQL_LOOKUP_ENTRY(proc_state, tty, UI32) },
    { "Nice",      PTQL_LOOKUP_ENTRY(proc_state, nice, UI32) },
    { "Priority",  PTQL_LOOKUP_ENTRY(proc_state, priority, UI32) },
    { "Threads",   PTQL_LOOKUP_ENTRY(proc_state, threads, UI64) },
    { "Processor", PTQL_LOOKUP_ENTRY(proc_state, processor, UI32) },
    { NULL }
};

static ptql_lookup_t PTQL_Fd[] = {
    { "Total", PTQL_LOOKUP_ENTRY(proc_fd, total, UI64) },
    { NULL }
};

static ptql_lookup_t PTQL_Args[] = {
    { NULL, ptql_args_match, 0, 0, PTQL_VALUE_TYPE_ANY, ptql_args_branch_init }
};

static ptql_lookup_t PTQL_Env[] = {
    { NULL, ptql_env_match, 0, 0, PTQL_VALUE_TYPE_ANY, ptql_branch_init_any }
};

static ptql_lookup_t PTQL_Port[] = {
    { NULL, ptql_port_match, 0, 0, PTQL_VALUE_TYPE_ANY, ptql_branch_init_port }
};

static ptql_lookup_t PTQL_Pid[] = {
    { NULL, ptql_pid_match, 0, 0, PTQL_VALUE_TYPE_ANY, ptql_branch_init_pid }
};

static ptql_entry_t ptql_map[] = {
    { "Time",     PTQL_Time },
    { "Cpu",      PTQL_Cpu },
    { "CredName", PTQL_CredName },
    { "Mem",      PTQL_Mem },
    { "Exe",      PTQL_Exe },
    { "Cred",     PTQL_Cred },
    { "State",    PTQL_State },
    { "Fd",       PTQL_Fd },
    { "Args",     PTQL_Args },
    { "Env",      PTQL_Env },
    { "Port",     PTQL_Port },
    { "Pid",      PTQL_Pid },
    { NULL }
};

static int ptql_branch_parse(char *query, ptql_parse_branch_t *branch,
                             sigar_ptql_error_t *error)
{
    char *ptr = strchr(query, '=');
    if (!ptr) {
        return ptql_error(error, "Missing '='");
    }

    branch->op_flags = 0;

    *ptr = '\0';
    branch->value = ++ptr;

    if ((ptr = strchr(query, '.'))) {
        *ptr = '\0';
        branch->name = query;
        query = ++ptr;
    }
    else {
        return ptql_error(error, "Missing '.'");
    }

    if ((ptr = strchr(query, '.'))) {
        *ptr = '\0';
        branch->attr = query;
        query = ++ptr;
    }
    else {
        return ptql_error(error, "Missing '.'");
    }

    if (*query) {
        char flag;

        while (sigar_isupper((flag = *query))) {
            switch (flag) {
              case 'P':
                branch->op_flags |= PTQL_OP_FLAG_PARENT;
                break;
              default:
                return ptql_error(error, "Unsupported modifier: %c", flag);
            }

            ++query;
        }

        branch->op = query;
    }
    else {
        return ptql_error(error, "Missing query");
    }

    return SIGAR_OK;
}

static int ptql_branch_add(ptql_parse_branch_t *parsed,
                           ptql_branch_list_t *branches,
                           sigar_ptql_error_t *error)
{
    ptql_branch_t *branch;
    ptql_entry_t *entry = NULL;
    ptql_lookup_t *lookup = NULL;
    int i, is_set=0;
    char *ptr;

    PTQL_BRANCH_LIST_GROW(branches);

    branch = &branches->data[branches->number++];
    SIGAR_ZERO(branch);
    branch->data_free = data_free;
    branch->value_free = data_free;
    branch->op_flags = parsed->op_flags;

    branch->op_name = ptql_op_code_get(parsed->op);
    if (branch->op_name == PTQL_OP_MAX) {
        return ptql_error(error, "Unsupported operator: %s", parsed->op);
    }

    for (i=0; ptql_map[i].name; i++) {
        if (strEQ(ptql_map[i].name, parsed->name)) {
            entry = &ptql_map[i];
            break;
        }
    }

    if (!entry) {
        return ptql_error(error, "Unsupported method: %s", parsed->name);
    }

    for (i=0; entry->members[i].name; i++) {
        if (strEQ(entry->members[i].name, parsed->attr)) {
            lookup = &entry->members[i];
            break;
        }
    }

    if (!lookup) {
        if (entry->members[0].type == PTQL_VALUE_TYPE_ANY) {
            /* Args, Env, etc. */
            lookup = &entry->members[0];
        }
        else {
            return ptql_error(error, "Unsupported %s attribute: %s",
                              parsed->name, parsed->attr);
        }
    }

    if (lookup->init) {
        int status = lookup->init(parsed, branch, error);
        if (status != SIGAR_OK) {
            return status;
        }
    }

    branch->lookup = lookup;

    if ((lookup->type < PTQL_VALUE_TYPE_STR) &&
        (branch->op_name > PTQL_OP_MAX_NSTR))
    {
        return ptql_error(error, "Unsupported operator '%s' for %s.%s",
                          parsed->op, parsed->name, parsed->attr);
    }

    if (*parsed->value == '$') {
        is_set = 1;

        if (branch->op_name == PTQL_OP_RE) {
            /* not for use with .re */
            return ptql_error(error, "Unsupported operator '%s' with variable %s",
                              parsed->op, parsed->value);
        }

        if (sigar_isdigit(*(parsed->value+1))) {
            branch->op_flags |= PTQL_OP_FLAG_REF;
            parsed->op_flags = branch->op_flags; /* for use by caller */
            branch->value.ui32 = atoi(parsed->value+1) - 1;

            if (branch->value.ui32 >= branches->number) {
                /* out-of-range */
                return ptql_error(error, "Variable %s out of range (%d)",
                                  parsed->value, branches->number);
            }
            else if (branch->value.ui32 == branches->number-1) {
                /* self reference */
                return ptql_error(error, "Variable %s self reference",
                                  parsed->value);
            }
        }
        else {
            if ((ptr = getenv(parsed->value+1))) {
                branch->value.str = sigar_strdup(ptr);
            }
            else {
                branch->value.str = NULL;
            }
        }
    }
    else if (branch->op_name == PTQL_OP_RE) {
#ifdef SIGAR_HAS_PCRE
        const char *error;
        int offset;
        pcre *re =
            pcre_compile(parsed->value, 0,
                         &error, &offset, NULL);
        if (!re) {
            /* XXX pcre_error ? */
            return ptql_error(error, "Invalid regex");
        }
        is_set = 1;
        branch->value.ptr = re;
        branch->value_free = pcre_free;
#endif
    }

    switch (lookup->type) {
      case PTQL_VALUE_TYPE_UI64:
        branch->match.ui64 = ptql_op_ui64[branch->op_name];
        if (!is_set) {
            branch->value.ui64 = strtoull(parsed->value, &ptr, 10);
            if (strtonum_failed(parsed->value, ptr)) {
                return PTQL_ERRNAN;
            }
        }
        break;
      case PTQL_VALUE_TYPE_UI32:
        branch->match.ui32 = ptql_op_ui32[branch->op_name];
        if (!is_set) {
            branch->value.ui32 = strtoul(parsed->value, &ptr, 10);
            if (strtonum_failed(parsed->value, ptr)) {
                return PTQL_ERRNAN;
            }
        }
        break;
      case PTQL_VALUE_TYPE_DBL:
        branch->match.dbl = ptql_op_dbl[branch->op_name];
        if (!is_set) {
            branch->value.dbl = strtod(parsed->value, &ptr);
            if (strtonum_failed(parsed->value, ptr)) {
                return PTQL_ERRNAN;
            }
        }
        break;
      case PTQL_VALUE_TYPE_CHR:
        branch->match.chr = ptql_op_chr[branch->op_name];
        if (!is_set) {
            if (strlen(parsed->value) != 1) {
                return ptql_error(error, "%s is not a char", parsed->value);
            }
            branch->value.chr[0] = parsed->value[0];
        }
        break;
      case PTQL_VALUE_TYPE_STR:
      case PTQL_VALUE_TYPE_ANY:
        branch->match.str = ptql_op_str[branch->op_name];
        if (!is_set) {
            branch->value.str = sigar_strdup(parsed->value);
        }
        break;
    }

    return SIGAR_OK;
}

static int ptql_branch_compare(const void *b1, const void *b2)
{
    /* XXX can do better */
    ptql_branch_t *branch1 = (ptql_branch_t *)b1;
    ptql_branch_t *branch2 = (ptql_branch_t *)b2;
    return
        branch1->lookup->type -
        branch2->lookup->type;
}

SIGAR_DECLARE(int) sigar_ptql_query_create(sigar_ptql_query_t **queryp,
                                           char *ptql,
                                           sigar_ptql_error_t *error)
{
    char *ptr, *ptql_copy = sigar_strdup(ptql);
    int status = SIGAR_OK;
    int has_ref = 0;
    sigar_ptql_query_t *query =
        *queryp = malloc(sizeof(*query));

    (void)ptql_error(error, "Malformed query");

#ifdef PTQL_DEBUG
    query->ptql = sigar_strdup(ptql);
#endif

    ptql = ptql_copy;

    ptql_branch_list_create(&query->branches);

    do {
        ptql_parse_branch_t parsed;

        if ((ptr = strchr(ptql, ','))) {
            *ptr = '\0';
        }

        status = ptql_branch_parse(ptql, &parsed, error);
        if (status == SIGAR_OK) {
            status =
                ptql_branch_add(&parsed, &query->branches, error);

            if (status != SIGAR_OK) {
                break;
            }
            if (parsed.op_flags & PTQL_OP_FLAG_REF) {
                has_ref = 1;
            }
        }
        else {
            break;
        }

        if (ptr) { 
            ptql = ++ptr;
        }
        else {
            break;
        }
    } while (*ptql);

    free(ptql_copy);

    if (status != SIGAR_OK) {
        sigar_ptql_query_destroy(query);
        *queryp = NULL;
    }
    else if (!has_ref && (query->branches.number > 1)) {
        qsort(query->branches.data,
              query->branches.number,
              sizeof(query->branches.data[0]),
              ptql_branch_compare);
    }

    if (status == SIGAR_OK) {
        (void)ptql_error(error, "OK");
    }
    return status;
}

SIGAR_DECLARE(int) sigar_ptql_query_destroy(sigar_ptql_query_t *query)
{
#ifdef PTQL_DEBUG
    free(query->ptql);
#endif
    ptql_branch_list_destroy(&query->branches);
    free(query);
    return SIGAR_OK;
}

SIGAR_DECLARE(void) sigar_ptql_re_impl_set(sigar_t *sigar, void *data,
                                           sigar_ptql_re_impl_t impl)
{
    sigar->ptql_re_data = data;
    sigar->ptql_re_impl = impl;
}

SIGAR_DECLARE(int) sigar_ptql_query_match(sigar_t *sigar,
                                          sigar_ptql_query_t *query,
                                          sigar_pid_t query_pid)
{
    int i;

    for (i=0; i<query->branches.number; i++) {
        sigar_pid_t pid = query_pid;
        int status, matched=0;
        ptql_branch_t *branch = &query->branches.data[i];
        ptql_lookup_t *lookup = branch->lookup;

        if (branch->op_flags & PTQL_OP_FLAG_PARENT) {
            sigar_proc_state_t state;

            status = sigar_proc_state_get(sigar, pid, &state);
            if (status != SIGAR_OK) {
                return status;
            }

            pid = state.ppid;
        }

        if (lookup->type == PTQL_VALUE_TYPE_ANY) {
            /* Args, Env, etc. */
            status = lookup->get(sigar, pid, branch);
            if (status == SIGAR_OK) {
                matched = 1;
            }
        }
        else {
            /* standard sigar_proc_*_get / structptr + offset */
            if (!branch->data.ptr) {
                branch->data_size = lookup->data_size;
                branch->data.ptr = malloc(branch->data_size);
            }
            status = lookup->get(sigar, pid, branch->data.ptr);
            if (status != SIGAR_OK) {
                return status;
            }

            if (branch->op_flags & PTQL_OP_FLAG_REF) {
                ptql_branch_t *ref =
                    &query->branches.data[branch->value.ui32];

                matched = ptql_branch_match_ref(branch, ref);
            }
#ifndef SIGAR_HAS_PCRE
            else if (branch->lookup->type == PTQL_VALUE_TYPE_STR) {
                matched = ptql_str_match(sigar, branch, (char *)DATA_PTR(branch));
            }
#endif
            else {
                matched = ptql_branch_match(branch);
            }
        }

        if (!matched) {
            return 1;
        }
    }

    return SIGAR_OK;
}

SIGAR_DECLARE(int) sigar_ptql_query_find_process(sigar_t *sigar,
                                                 sigar_ptql_query_t *query,
                                                 sigar_pid_t *pid)
{
    int status;
    int i, matches=0;
    sigar_proc_list_t *pids;

    if ((query->branches.number == 1) &&
        (query->branches.data[0].op_flags & PTQL_OP_FLAG_PID))
    {
        /* avoid scanning the process list for single Pid.* queries */
        return ptql_pid_get(sigar, &query->branches.data[0], pid);
    }

    status = sigar_proc_list_get(sigar, NULL);
    if (status != SIGAR_OK) {
        return status;
    }
    pids = sigar->pids;

    for (i=0; i<pids->number; i++) {
        int query_status =
            sigar_ptql_query_match(sigar, query, pids->data[i]);

        if (query_status == SIGAR_OK) {
            *pid = pids->data[i];
            matches++;
        }
        else if (query_status == SIGAR_ENOTIMPL) {
            /* let caller know query is invalid. */
            status = query_status;
            break;
        } /* else ok, e.g. permission denied */
    }

    if (status != SIGAR_OK) {
        return status;
    }

    if (matches == 1) {
        return SIGAR_OK;
    }
    else if (matches == 0) {
        sigar_strerror_set(sigar,
                           "Query did not match any processes");
    }
    else {
        sigar_strerror_printf(sigar,
                              "Query matched multiple processes (%d)",
                              matches);
    }

    return -1;
}

SIGAR_DECLARE(int) sigar_ptql_query_find(sigar_t *sigar,
                                         sigar_ptql_query_t *query,
                                         sigar_proc_list_t *proclist)
{
    int status;
    int i;
    sigar_proc_list_t *pids;

    status = sigar_proc_list_get(sigar, NULL);
    if (status != SIGAR_OK) {
        return status;
    }
    pids = sigar->pids;

    sigar_proc_list_create(proclist);

    for (i=0; i<pids->number; i++) {
        int query_status =
            sigar_ptql_query_match(sigar, query, pids->data[i]);

        if (query_status == SIGAR_OK) {
            SIGAR_PROC_LIST_GROW(proclist);
            proclist->data[proclist->number++] = pids->data[i];
        }
        else if (query_status == SIGAR_ENOTIMPL) {
            /* let caller know query is invalid. */
            status = query_status;
            break;
        }
    }

    if (status != SIGAR_OK) {
        sigar_proc_list_destroy(sigar, proclist);
        return status;
    }

    return SIGAR_OK;
}
