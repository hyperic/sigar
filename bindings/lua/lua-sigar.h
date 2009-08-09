/**
 * Copyright (c) 2009, Sun Microsystems Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - Neither the name of Sun Microsystems Inc. nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef __LUA_SIGAR_H__
#define __LUA_SIGAR_H__

#include "sigar.h"

#define LUA_EXPORT_INT(x, y) \
	if (x->y == SIGAR_FIELD_NOTIMPL) { \
		lua_pushnil(L); \
	} else { \
		lua_pushnumber(L, x->y); \
	} \
	lua_setfield(L, -2, #y);

#define LUA_EXPORT_DOUBLE(x, y) \
	lua_pushnumber(L, x->y); \
	lua_setfield(L, -2, #y);

#define LUA_EXPORT_STR(x, y) \
	lua_pushstring(L, x->y); \
	lua_setfield(L, -2, #y);

#define LUA_EXPORT_ADDRESS(x, y) \
	lua_sigar_push_address(L, &(x->y)); \
	lua_setfield(L, -2, #y);

int lua_sigar_cpus_get(lua_State *L);
int lua_sigar_mem_get(lua_State *L);
int lua_sigar_swap_get(lua_State *L);
int lua_sigar_procs_get(lua_State *L);
int lua_sigar_proc_get(lua_State *L);
int lua_sigar_pid_get(lua_State *L);
int lua_sigar_fses_get(lua_State *L);
int lua_sigar_disk_get(lua_State *L);
int lua_sigar_disks_get(lua_State *L);
int lua_sigar_netifs_get(lua_State *L);
int lua_sigar_who_get(lua_State *L);
int lua_sigar_version_get(lua_State *L);
int lua_sigar_sysinfo_get(lua_State *L);

int lua_sigar_push_address(lua_State *L, sigar_net_address_t *addr);

#endif
