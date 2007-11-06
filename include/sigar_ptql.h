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

#ifndef SIGAR_PTQL_H
#define SIGAR_PTQL_H

#define SIGAR_PTQL_MALFORMED_QUERY -1

typedef struct sigar_ptql_query_t sigar_ptql_query_t;

#define SIGAR_PTQL_ERRMSG_SIZE 1024

typedef struct {
    char message[SIGAR_PTQL_ERRMSG_SIZE];
} sigar_ptql_error_t;

typedef int (*sigar_ptql_re_impl_t)(void *, char *, char *);

SIGAR_DECLARE(void) sigar_ptql_re_impl_set(sigar_t *sigar, void *data,
                                           sigar_ptql_re_impl_t impl);

SIGAR_DECLARE(int) sigar_ptql_query_create(sigar_ptql_query_t **query,
                                           char *ptql,
                                           sigar_ptql_error_t *error);

SIGAR_DECLARE(int) sigar_ptql_query_match(sigar_t *sigar,
                                          sigar_ptql_query_t *query,
                                          sigar_pid_t pid);

SIGAR_DECLARE(int) sigar_ptql_query_destroy(sigar_ptql_query_t *query);

SIGAR_DECLARE(int) sigar_ptql_query_find_process(sigar_t *sigar,
                                                 sigar_ptql_query_t *query,
                                                 sigar_pid_t *pid);

SIGAR_DECLARE(int) sigar_ptql_query_find(sigar_t *sigar,
                                         sigar_ptql_query_t *query,
                                         sigar_proc_list_t *proclist);

#endif /*SIGAR_PTQL_H*/
