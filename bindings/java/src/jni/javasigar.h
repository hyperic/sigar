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

#define JENV (*env)

#define SIGAR_PACKAGE "org/hyperic/sigar/"

#define SIGAR_JNI(m) JNICALL Java_org_hyperic_sigar_##m

#define SIGAR_JNIx(m) JNICALL Java_org_hyperic_sigar_Sigar_##m

#define SIGAR_FIND_CLASS(name) \
    JENV->FindClass(env, SIGAR_PACKAGE name)

#define SIGAR_CLASS_SIG(name) \
    "L" SIGAR_PACKAGE name ";"

