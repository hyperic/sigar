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
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <string.h>
#include <stdlib.h>

#include "sigar.h"
#include "lua-sigar.h"

int lua_sigar_sysinfo_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	sigar_sys_info_t sysinfo;

	sigar_sys_info_get(s, &sysinfo);

	lua_newtable(L);
#define DATA \
	(&(sysinfo))

	LUA_EXPORT_STR(DATA, name);
	LUA_EXPORT_STR(DATA, version);
	LUA_EXPORT_STR(DATA, arch);
	LUA_EXPORT_STR(DATA, machine);
	LUA_EXPORT_STR(DATA, description);
	LUA_EXPORT_STR(DATA, patch_level);
	LUA_EXPORT_STR(DATA, vendor);
	LUA_EXPORT_STR(DATA, vendor_version);
	LUA_EXPORT_STR(DATA, vendor_name);
	LUA_EXPORT_STR(DATA, vendor_code_name);

#undef DATA

	return 1;
}


