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

typedef struct {
	sigar_who_list_t who;

	sigar_t *sigar;
} lua_sigar_who_t;

static int lua_sigar_who_gc(lua_State *L) {
	lua_sigar_who_t *who = (lua_sigar_who_t *)luaL_checkudata(L, 1, "sigar_who");

	sigar_who_list_destroy(who->sigar, &(who->who));

	return 0;
}

static int lua_sigar_who_len(lua_State *L) {
	lua_sigar_who_t *who = (lua_sigar_who_t *)luaL_checkudata(L, 1, "sigar_who");

	lua_pushinteger(L, who->who.number);

	return 1;
}

static int lua_sigar_who_get_who(lua_State *L) {
	lua_sigar_who_t *who = (lua_sigar_who_t *)luaL_checkudata(L, 1, "sigar_who");
	int ndx = luaL_checkint(L, 2);

	if (ndx - 1 < 0 || 
	    ndx - 1 >= who->who.number) {
		luaL_error(L, ".who[%d] out of range: 1..%d", ndx, who->who.number);
	}

	lua_newtable(L);
#define DATA \
	(&(who->who.data[ndx - 1]))

	LUA_EXPORT_STR(DATA, user);
	LUA_EXPORT_STR(DATA, device);
	LUA_EXPORT_STR(DATA, host);
	LUA_EXPORT_INT(DATA, time);
#undef DATA

	return 1;
}

int lua_sigar_who_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	lua_sigar_who_t *who;

	who = lua_newuserdata(L, sizeof(lua_sigar_who_t));
	who->sigar = s;
	sigar_who_list_get(s, &(who->who));
	if (0 != luaL_newmetatable(L, "sigar_who")) {
		lua_pushcfunction(L, lua_sigar_who_len);
		lua_setfield(L, -2, "__len");
		lua_pushcfunction(L, lua_sigar_who_get_who);
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_who_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}


