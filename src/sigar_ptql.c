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

#ifdef WIN32
#define strtoull strtoul /*XXX*/
#endif

/* XXX need more specific errors */
#define SIGAR_PTQL_MALFORMED_QUERY 1

typedef struct ptql_parse_branch_t ptql_parse_branch_t;
typedef struct ptql_branch_t ptql_branch_t;

typedef int (*ptql_get_t)(sigar_t *sigar, sigar_pid_t pid, void *data);
typedef int (*ptql_branch_init_t)(ptql_parse_branch_t *parsed, ptql_branch_t *branch);

typedef int (*ptql_op_ui64_t)(sigar_uint64_t haystack, sigar_uint64_t needle);
typedef int (*ptql_op_ui32_t)(sigar_uint32_t haystack, sigar_uint32_t needle);
typedef int (*ptql_op_str_t)(char *haystack, char *needle);
typedef int (*ptql_op_chr_t)(char haystack, char needle);

typedef enum {
    PTQL_VALUE_TYPE_UI64,
    PTQL_VALUE_TYPE_UI32,
    PTQL_VALUE_TYPE_CHR,
    PTQL_VALUE_TYPE_STR,
    PTQL_VALUE_TYPE_ANY
} ptql_value_type_t;

#define PTQL_OP_FLAG_PARENT 1
#define PTQL_OP_FLAG_REF    2

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
    ((char *)branch->data + branch->lookup->offset)

struct ptql_branch_t {
    ptql_lookup_t *lookup;
    void *data;
    unsigned int data_size;
    unsigned int flags;
    unsigned int op_flags;
    union {
        ptql_op_ui64_t ui64;
        ptql_op_ui32_t ui32;
        ptql_op_chr_t chr;
        ptql_op_str_t str;
    } match;
    union {
        sigar_uint64_t ui64;
        sigar_uint32_t ui32;
        char chr[4];
        char *str;
    } value;
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
};

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

static int ptql_op_ui64_eq(sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack == needle;
}

static int ptql_op_ui64_ne(sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack != needle;
}

static int ptql_op_ui64_gt(sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack > needle;
}

static int ptql_op_ui64_ge(sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack >= needle;
}

static int ptql_op_ui64_lt(sigar_uint64_t haystack, sigar_uint64_t needle)
{
    return haystack < needle;
}

static int ptql_op_ui64_le(sigar_uint64_t haystack, sigar_uint64_t needle)
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

static int ptql_op_ui32_eq(sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack == needle;
}

static int ptql_op_ui32_ne(sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack != needle;
}

static int ptql_op_ui32_gt(sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack > needle;
}

static int ptql_op_ui32_ge(sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack >= needle;
}

static int ptql_op_ui32_lt(sigar_uint32_t haystack, sigar_uint32_t needle)
{
    return haystack < needle;
}

static int ptql_op_ui32_le(sigar_uint32_t haystack, sigar_uint32_t needle)
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

static int ptql_op_str_eq(char *haystack, char *needle)
{
    return strEQ(haystack, needle);
}

static int ptql_op_str_ne(char *haystack, char *needle)
{
    return !strEQ(haystack, needle);
}

static int ptql_op_str_gt(char *haystack, char *needle)
{
    return strcmp(haystack, needle) > 0;
}

static int ptql_op_str_ge(char *haystack, char *needle)
{
    return strcmp(haystack, needle) >= 0;
}

static int ptql_op_str_lt(char *haystack, char *needle)
{
    return strcmp(haystack, needle) < 0;
}

static int ptql_op_str_le(char *haystack, char *needle)
{
    return strcmp(haystack, needle) <= 0;
}

static int ptql_op_str_ew(char *haystack, char *needle)
{
    int nlen = strlen(needle);
    int hlen = strlen(haystack);
    int diff = hlen - nlen;
    if (diff < 0) {
        return 0;
    }
    return strnEQ(haystack + diff, needle, nlen);
}

static int ptql_op_str_sw(char *haystack, char *needle)
{
    return strnEQ(haystack, needle, strlen(needle));
}

static int ptql_op_str_re(char *haystack, char *needle)
{
    return 0; /*XXX pcre?*/
}

static int ptql_op_str_ct(char *haystack, char *needle)
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

static int ptql_op_chr_eq(char haystack, char needle)
{
    return haystack == needle;
}

static int ptql_op_chr_ne(char haystack, char needle)
{
    return haystack != needle;
}

static int ptql_op_chr_gt(char haystack, char needle)
{
    return haystack > needle;
}

static int ptql_op_chr_ge(char haystack, char needle)
{
    return haystack >= needle;
}

static int ptql_op_chr_lt(char haystack, char needle)
{
    return haystack < needle;
}

static int ptql_op_chr_le(char haystack, char needle)
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

static int ptql_branch_list_destroy(sigar_t *sigar,
                                    ptql_branch_list_t *branches)
{
    if (branches->size) {
        int i;

        for (i=0; i<branches->number; i++) {
            ptql_branch_t *branch =
                &branches->data[i];

            if (branch->data_size && branch->data) {
                free(branch->data);
            }

            if (branch->lookup &&
                (branch->lookup->type == PTQL_VALUE_TYPE_STR) &&
                !(branch->op_flags & PTQL_OP_FLAG_REF))
            {
                if (branch->value.str) {
                    free(branch->value.str);
                }
            }
        }

        free(branches->data);
        branches->number = branches->size = 0;
    }

    return SIGAR_OK;
}

static int ptql_branch_init_any(ptql_parse_branch_t *parsed,
                                ptql_branch_t *branch)
{
    branch->data = strdup(parsed->attr);
    branch->data_size = strlen(parsed->attr);
    return SIGAR_OK;
}

static int ptql_branch_match(ptql_branch_t *branch)
{
    switch (branch->lookup->type) {
      case PTQL_VALUE_TYPE_UI64:
        return branch->match.ui64(*(sigar_uint64_t *)DATA_PTR(branch),
                                  branch->value.ui64);
      case PTQL_VALUE_TYPE_UI32:
        return branch->match.ui32(*(sigar_uint32_t *)DATA_PTR(branch),
                                  branch->value.ui32);
      case PTQL_VALUE_TYPE_CHR:
        return branch->match.chr(*(char *)DATA_PTR(branch),
                                 branch->value.chr[0]);
      case PTQL_VALUE_TYPE_STR:
      case PTQL_VALUE_TYPE_ANY:
        return branch->match.str((char *)DATA_PTR(branch),
                                 branch->value.str);
      default:
        return 0;
    }
}

static int ptql_branch_match_ref(ptql_branch_t *branch, ptql_branch_t *ref)
{
    switch (branch->lookup->type) {
      case PTQL_VALUE_TYPE_UI64:
        return branch->match.ui64(*(sigar_uint64_t *)DATA_PTR(branch),
                                  *(sigar_uint64_t *)DATA_PTR(ref));
      case PTQL_VALUE_TYPE_UI32:
        return branch->match.ui32(*(sigar_uint32_t *)DATA_PTR(branch),
                                  *(sigar_uint32_t *)DATA_PTR(ref));
      case PTQL_VALUE_TYPE_CHR:
        return branch->match.chr(*(char *)DATA_PTR(branch),
                                 *(char *)DATA_PTR(ref));
      case PTQL_VALUE_TYPE_STR:
      case PTQL_VALUE_TYPE_ANY:
        return branch->match.str((char *)DATA_PTR(branch),
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

static int ptql_branch_init_pid(ptql_parse_branch_t *parsed,
                                ptql_branch_t *branch)
{
    if (strEQ(parsed->attr, "Pid")) {
        branch->flags = PTQL_PID_PID;
        if (strEQ(parsed->value, "$$")) {
            branch->data = (void*)getpid();
        }
        else {
            branch->data = (void*)atoi(parsed->value); /*XXX*/
        }
        return SIGAR_OK;
    }
    else if (strEQ(parsed->attr, "PidFile")) {
        branch->flags = PTQL_PID_FILE;
        branch->data = strdup(parsed->value);
        branch->data_size = strlen(parsed->value);
        return SIGAR_OK;
    }
    else if (strEQ(parsed->attr, "Service")) {
#ifdef WIN32
        branch->flags = PTQL_PID_SERVICE;
        branch->data = strdup(parsed->value);
        branch->data_size = strlen(parsed->value);
        return SIGAR_OK;
#else
        return SIGAR_PTQL_MALFORMED_QUERY;
#endif
    }
    else {
        return SIGAR_PTQL_MALFORMED_QUERY;
    }
}

static int ptql_pid_match(sigar_t *sigar,
                          sigar_pid_t pid,
                          void *data)
{
    ptql_branch_t *branch =
        (ptql_branch_t *)data;
    sigar_pid_t match_pid;

    if (branch->flags == PTQL_PID_FILE) {
        char buffer[SIGAR_PATH_MAX+1];
        int status =
            sigar_file2str((const char *)branch->data,
                           buffer, sizeof(buffer)-1);
        if (status != SIGAR_OK) {
            return status;
        }
        match_pid = strtoull(buffer, NULL, 10); /*XXX validate*/
    }
    else if (branch->flags == PTQL_PID_SERVICE) {
#ifdef WIN32
        return !SIGAR_OK; /*XXX*/
#else
        return !SIGAR_OK;
#endif
    }
    else {
        match_pid = (sigar_pid_t)branch->data;
    }

    return (pid == match_pid) ? SIGAR_OK : !SIGAR_OK;
}

static int ptql_args_branch_init(ptql_parse_branch_t *parsed,
                                 ptql_branch_t *branch)
{
    if (strEQ(parsed->attr, "*")) {
        branch->data = NULL;
    }
    else {
        char *end;

        branch->data =
            (void*)strtol(parsed->attr, &end, 10);

        if (end && *end) {
            /* conversion failed */
            return SIGAR_PTQL_MALFORMED_QUERY;
        }
    }
    return SIGAR_OK;
}

static int ptql_args_match(sigar_t *sigar,
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

    if (!branch->data) {
        int i;
        for (i=0; i<args.number; i++) {
            matched = 
                branch->match.str(args.data[i],
                                  branch->value.str);
            if (matched) {
                break;
            }
        }
    }
    else {
        int num = (int)branch->data;

        /* e.g. find last element of args: Args.-1.eq=weblogic.Server */
        if (num < 0) {
            num += args.number;
        }
        if ((num >= 0) && (num < args.number)) {
            matched = 
                branch->match.str(args.data[num],
                                  branch->value.str);
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

static int ptql_env_match(sigar_t *sigar,
                          sigar_pid_t pid,
                          void *data)
{
    ptql_branch_t *branch =
        (ptql_branch_t *)data;
    int status, matched=0;
    sigar_proc_env_t procenv;
    sigar_proc_env_entry_t entry;

    /* XXX ugh this is klunky */
    entry.key = branch->data;
    entry.klen = branch->data_size;
    entry.val = NULL;

    procenv.type = SIGAR_PROC_ENV_KEY;
    procenv.key  = branch->data;
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
                branch->match.str(entry.val,
                                  branch->value.str);
        }
    }

    return matched ? SIGAR_OK : !SIGAR_OK;
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

static ptql_lookup_t PTQL_Pid[] = {
    { NULL, ptql_pid_match, 0, 0, PTQL_VALUE_TYPE_ANY, ptql_branch_init_pid }
};

static ptql_entry_t ptql_map[] = {
    { "Time",     PTQL_Time },
    { "CredName", PTQL_CredName },
    { "Mem",      PTQL_Mem },
    { "Exe",      PTQL_Exe },
    { "Cred",     PTQL_Cred },
    { "State",    PTQL_State },
    { "Fd",       PTQL_Fd },
    { "Args",     PTQL_Args },
    { "Env",      PTQL_Env },
    { "Pid",      PTQL_Pid },
    { NULL }
};

static int ptql_branch_parse(char *query, ptql_parse_branch_t *branch)
{
    char *ptr = strchr(query, '=');
    if (!ptr) {
        return SIGAR_PTQL_MALFORMED_QUERY;
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
        return SIGAR_PTQL_MALFORMED_QUERY;
    }

    if ((ptr = strchr(query, '.'))) {
        *ptr = '\0';
        branch->attr = query;
        query = ++ptr;
    }
    else {
        return SIGAR_PTQL_MALFORMED_QUERY;
    }

    if (*query) {
        char flag;

        while (isupper((flag = *query))) {
            switch (flag) {
              case 'P':
                branch->op_flags |= PTQL_OP_FLAG_PARENT;
                break;
              default:
                return SIGAR_PTQL_MALFORMED_QUERY;
            }

            ++query;
        }

        branch->op = query;
    }
    else {
        return SIGAR_PTQL_MALFORMED_QUERY;
    }

    return SIGAR_OK;
}

static int ptql_branch_add(ptql_parse_branch_t *parsed,
                           ptql_branch_list_t *branches)
{
    ptql_branch_t *branch;
    ptql_entry_t *entry = NULL;
    ptql_lookup_t *lookup = NULL;
    ptql_op_name_t op;
    int i, is_ref=0;

    PTQL_BRANCH_LIST_GROW(branches);

    branch = &branches->data[branches->number++];
    branch->data = NULL;
    branch->data_size = 0;
    branch->op_flags = parsed->op_flags;

    op = ptql_op_code_get(parsed->op);
    if (op == PTQL_OP_MAX) {
        return SIGAR_PTQL_MALFORMED_QUERY;
    }

    for (i=0; ptql_map[i].name; i++) {
        if (strEQ(ptql_map[i].name, parsed->name)) {
            entry = &ptql_map[i];
            break;
        }
    }

    if (!entry) {
        return SIGAR_PTQL_MALFORMED_QUERY;
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
            return SIGAR_PTQL_MALFORMED_QUERY;
        }
    }

    if (lookup->init) {
        int status = lookup->init(parsed, branch);
        if (status != SIGAR_OK) {
            return status;
        }
    }

    branch->lookup = lookup;

    if ((lookup->type < PTQL_VALUE_TYPE_STR) &&
        (op > PTQL_OP_MAX_NSTR))
    {
        return SIGAR_PTQL_MALFORMED_QUERY;
    }

    if ((*parsed->value == '$') &&
        sigar_isdigit(*(parsed->value+1)))
    {
        is_ref = 1;
        branch->op_flags |= PTQL_OP_FLAG_REF;
        branch->value.ui32 = atoi(parsed->value+1) - 1;

        if (branch->value.ui32 >= branches->number) {
            /* out-of-range */
            return SIGAR_PTQL_MALFORMED_QUERY;
        }
        else if (branch->value.ui32 == branches->number-1) {
            /* self reference */
            return SIGAR_PTQL_MALFORMED_QUERY;
        }
    }

    switch (lookup->type) {
      case PTQL_VALUE_TYPE_UI64:
        branch->match.ui64 = ptql_op_ui64[op];
        if (!is_ref) {
            branch->value.ui64 = strtoull(parsed->value, NULL, 10);
        }
        break;
      case PTQL_VALUE_TYPE_UI32:
        branch->match.ui32 = ptql_op_ui32[op];
        if (!is_ref) {
            branch->value.ui32 = strtoul(parsed->value, NULL, 10);
        }
        break;
      case PTQL_VALUE_TYPE_CHR:
        branch->match.chr = ptql_op_chr[op];
        if (!is_ref) {
            branch->value.chr[0] = parsed->value[0];
        }
        break;
      case PTQL_VALUE_TYPE_STR:
      case PTQL_VALUE_TYPE_ANY:
        branch->match.str = ptql_op_str[op];
        if (!is_ref) {
            branch->value.str = strdup(parsed->value);
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

SIGAR_DECLARE(int) sigar_ptql_query_create(sigar_t *sigar,
                                           sigar_ptql_query_t **queryp,
                                           char *ptql)
{
    char *ptr, *ptql_copy = strdup(ptql);
    int status = SIGAR_OK;
    sigar_ptql_query_t *query =
        *queryp = malloc(sizeof(*query));

    ptql = ptql_copy;

    ptql_branch_list_create(&query->branches);

    do {
        ptql_parse_branch_t parsed;

        if ((ptr = strchr(ptql, ','))) {
            *ptr = '\0';
        }

        status = ptql_branch_parse(ptql, &parsed);
        if (status == SIGAR_OK) {
            status =
                ptql_branch_add(&parsed, &query->branches);

            if (status != SIGAR_OK) {
                break;
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
        sigar_ptql_query_destroy(sigar, query);
        *queryp = NULL;
    }
    else if (query->branches.number > 1) {
        qsort(query->branches.data,
              query->branches.number,
              sizeof(query->branches.data[0]),
              ptql_branch_compare);
    }

    return status;
}

SIGAR_DECLARE(int) sigar_ptql_query_destroy(sigar_t *sigar,
                                            sigar_ptql_query_t *query)
{
    ptql_branch_list_destroy(sigar, &query->branches);
    free(query);
    return SIGAR_OK;
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
            if (!branch->data) {
                branch->data_size = lookup->data_size;
                branch->data = malloc(branch->data_size);
            }
            status = lookup->get(sigar, pid, branch->data);
            if (status != SIGAR_OK) {
                return status;
            }

            if (branch->op_flags & PTQL_OP_FLAG_REF) {
                ptql_branch_t *ref =
                    &query->branches.data[branch->value.ui32];

                matched = ptql_branch_match_ref(branch, ref);
            }
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
