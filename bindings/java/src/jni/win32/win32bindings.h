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

#ifndef WIN32_BINDINGS_H
#define WIN32_BINDINGS_H

/* Exclude rarely-used stuff from windows headers */
#define WIN32_LEAN_AND_MEAN

/* Windows Header Files */
#include <windows.h>

#include <tchar.h>

/* Include java jni headers */
#include <jni.h>

#define WIN32_PACKAGE "org/hyperic/sigar/win32/"

#define WIN32_FIND_CLASS(name) \
    JENV->FindClass(env, WIN32_PACKAGE name)

#define WIN32_ALLOC_OBJECT(name) \
    JENV->AllocObject(env, WIN32_FIND_CLASS(name))

#define SetStringField(env, obj, fieldID, val) \
    JENV->SetObjectField(env, obj, fieldID, JENV->NewStringUTF(env, val))

#ifdef __cplusplus
extern "C" {
#endif
    void win32_throw_exception(JNIEnv *env, char *msg);

    void win32_throw_error(JNIEnv *env, LONG err);

    void win32_throw_last_error(JNIEnv *env);
#ifdef __cplusplus
}
#endif
#endif
