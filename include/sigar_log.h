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

#ifndef SIGAR_LOG_H
#define SIGAR_LOG_H

#include <stdarg.h>

#define	SIGAR_LOG_FATAL  0
#define	SIGAR_LOG_ERROR  1
#define	SIGAR_LOG_WARN   2
#define	SIGAR_LOG_INFO   3
#define	SIGAR_LOG_DEBUG  4
#define	SIGAR_LOG_TRACE  5

#define SIGAR_LOG_IS_FATAL(sigar) \
    (sigar->log_level >= SIGAR_LOG_FATAL)

#define SIGAR_LOG_IS_ERROR(sigar) \
    (sigar->log_level >= SIGAR_LOG_ERROR)

#define SIGAR_LOG_IS_WARN(sigar) \
    (sigar->log_level >= SIGAR_LOG_WARN)

#define SIGAR_LOG_IS_INFO(sigar) \
    (sigar->log_level >= SIGAR_LOG_INFO)

#define SIGAR_LOG_IS_DEBUG(sigar) \
    (sigar->log_level >= SIGAR_LOG_DEBUG)

#define SIGAR_LOG_IS_TRACE(sigar) \
    (sigar->log_level >= SIGAR_LOG_TRACE)

#define SIGAR_STRINGIFY(n) #n

#define SIGAR_LOG_FILELINE \
   __FILE__ ":" SIGAR_STRINGIFY(__LINE__)

#if defined(__GNUC__)
#   if (__GNUC__ > 2)
#      define SIGAR_FUNC __func__
#   else
#      define SIGAR_FUNC __FUNCTION__
#   endif
#else
#   define SIGAR_FUNC SIGAR_LOG_FILELINE
#endif

typedef void (*sigar_log_impl_t)(sigar_t *, void *, int, char *);

SIGAR_DECLARE(void) sigar_log_printf(sigar_t *sigar, int level,
                                     const char *format, ...);

SIGAR_DECLARE(void) sigar_log(sigar_t *sigar, int level, char *message);

SIGAR_DECLARE(void) sigar_log_impl_set(sigar_t *sigar, void *data,
                                       sigar_log_impl_t impl);

SIGAR_DECLARE(void) sigar_log_impl_file(sigar_t *sigar, void *data,
                                        int level, char *message);

SIGAR_DECLARE(int) sigar_log_level_get(sigar_t *sigar);

SIGAR_DECLARE(void) sigar_log_level_set(sigar_t *sigar, int level);


#endif /* SIGAR_LOG_H */
