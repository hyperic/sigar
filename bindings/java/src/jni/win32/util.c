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

#ifdef WIN32

#include <pdh.h>
#include <pdhmsg.h>

#include "sigar.h"
#include "sigar_private.h"
#include "win32bindings.h"
#include "javasigar.h"

/**
 * Set of common utilities for all win32bindings objects
 */

#ifdef __cplusplus
extern "C" {
#endif

void win32_throw_exception(JNIEnv *env, char *msg)
{
    jclass exceptionClass = WIN32_FIND_CLASS("Win32Exception");
    JENV->ThrowNew(env, exceptionClass, msg);
}

void win32_throw_last_error(JNIEnv *env)
{
    win32_throw_error(env, GetLastError());
}

void win32_throw_error(JNIEnv *env, LONG err)
{
    char msg[8192];

    win32_throw_exception(env,
                          sigar_strerror_get(err, msg, sizeof(msg)));
}

#ifdef __cplusplus
}
#endif
#endif /* WIN32 */
