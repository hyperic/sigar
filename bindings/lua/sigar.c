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

/**
 * push the converted sigar_net_address_t as string on the stack
 */
int lua_sigar_push_address(lua_State *L, sigar_net_address_t *addr) {
	char s[24 + 1]; /* AF_LINK  is 2 * 8 + 7 colons = 23
			   AF_INET6 is 4 * 4 + 3 colons = 17
			   AF_INET  is 4 * 3 + 3 dots = 15
			   */
	size_t s_have = sizeof(s) - 1;
	size_t s_need;

	switch (addr->family) {
	case SIGAR_AF_UNSPEC:
		lua_pushnil(L);
		return 1;
	case SIGAR_AF_INET:
		lua_pushfstring(L, "%d.%d.%d.%d",
				(addr->addr.in >> 0) & 0xff,
				(addr->addr.in >> 8) & 0xff,
				(addr->addr.in >> 16) & 0xff,
				(addr->addr.in >> 24) & 0xff);
		return 1;
	case SIGAR_AF_INET6:
		s_need = snprintf(s, s_have, "%4x:%4x:%4x:%4x",
				(addr->addr.in6[0]),
				(addr->addr.in6[1]),
				(addr->addr.in6[2]),
				(addr->addr.in6[3]));

		if (s_need > s_have) {
			/* string is truncated, but written to s */
			luaL_error(L, "can't convert INET6 address string, not enough memory: %d need, %d available",
					s_need, s_have);
		}
		lua_pushstring(L, s);
		return 1;
	case SIGAR_AF_LINK:
		s_need = snprintf(s, s_have, "%02x:%02x:%02x:%02x:%02x:%02x:%02x:%02x",
				addr->addr.mac[0],
				addr->addr.mac[1],
				addr->addr.mac[2],
				addr->addr.mac[3],
				addr->addr.mac[4],
				addr->addr.mac[5],
				addr->addr.mac[6],
				addr->addr.mac[7]);

		if (s_need > s_have) {
			/* string is truncated, but written to s */
			luaL_error(L, "can't convert MAC address string, not enough memory: %d need, %d available",
					s_need, s_have);
		}
		lua_pushstring(L, s);
		return 1;
	}
	return 0;
}

static int lua_sigar_free(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");

	sigar_close(s);

	return 0;
}

static int lua_sigar_new(lua_State *L) {
	sigar_t **_s;
	sigar_t *s;

	if (SIGAR_OK != sigar_open(&s)) {
		luaL_error(L, "sigar_open() failed");
		return 0;
	}

	_s = lua_newuserdata(L, sizeof(sigar_t *));
	*_s = s;

	if (0 != luaL_newmetatable(L, "sigar")) {
		lua_newtable(L);
		lua_pushcfunction(L, lua_sigar_cpus_get);
		lua_setfield(L, -2, "cpus");
		lua_pushcfunction(L, lua_sigar_procs_get);
		lua_setfield(L, -2, "procs");
		lua_pushcfunction(L, lua_sigar_fses_get);
		lua_setfield(L, -2, "filesystems");
		lua_pushcfunction(L, lua_sigar_disks_get);
		lua_setfield(L, -2, "disks");
		lua_pushcfunction(L, lua_sigar_disk_get);
		lua_setfield(L, -2, "disk");
		lua_pushcfunction(L, lua_sigar_who_get);
		lua_setfield(L, -2, "who");
		lua_pushcfunction(L, lua_sigar_netifs_get);
		lua_setfield(L, -2, "netifs");
		lua_pushcfunction(L, lua_sigar_proc_get);
		lua_setfield(L, -2, "proc");
		lua_pushcfunction(L, lua_sigar_pid_get);
		lua_setfield(L, -2, "pid");
		lua_pushcfunction(L, lua_sigar_mem_get);
		lua_setfield(L, -2, "mem");
		lua_pushcfunction(L, lua_sigar_swap_get);
		lua_setfield(L, -2, "swap");
		lua_pushcfunction(L, lua_sigar_version_get);
		lua_setfield(L, -2, "version");
		lua_pushcfunction(L, lua_sigar_sysinfo_get);
		lua_setfield(L, -2, "sysinfo");
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_free);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}

/*
** Assumes the table is on top of the stack.
*/
static void set_info (lua_State *L) {
	lua_pushliteral (L, "_COPYRIGHT");
	lua_pushliteral (L, "Copyright (c) 2009 Sun Microsystems, Inc.");
	lua_settable (L, -3);
	lua_pushliteral (L, "_DESCRIPTION");
	lua_pushliteral (L, "sigar.*");
	lua_settable (L, -3);
	lua_pushliteral (L, "_VERSION");
	lua_pushliteral (L, "LuaSigar 0.1");
	lua_settable (L, -3);
}


static const struct luaL_reg sigarlib[] = {
	{"new", lua_sigar_new},
	{NULL, NULL}
};

#if defined(_WIN32)
# define LUAEXT_API __declspec(dllexport)
#else
# define LUAEXT_API extern
#endif

LUAEXT_API int luaopen_sigar (lua_State *L) {
	luaL_register (L, "sigar", sigarlib);
	set_info (L);
	return 1;
}
