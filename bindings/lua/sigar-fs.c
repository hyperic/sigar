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

#include "sigar.h"
#include "lua-sigar.h"

/* file-systems */
typedef struct {
	sigar_file_system_list_t fses;

	sigar_t *sigar;

	int ref_count;
} lua_sigar_fses_t;

typedef struct {
	lua_sigar_fses_t *fses;

	int ndx;
} lua_sigar_fs_t;

static int lua_sigar_fses_free(lua_State *L, lua_sigar_fses_t *fses) {
	if (--fses->ref_count == 0) {
		sigar_file_system_list_destroy(fses->sigar, &(fses->fses));
	}

	return 0;
}

static int lua_sigar_fses_gc(lua_State *L) {
	lua_sigar_fses_t *fses = (lua_sigar_fses_t *)luaL_checkudata(L, 1, "sigar_fses");

	lua_sigar_fses_free(L, fses);

	return 0;
}

static int lua_sigar_fses_len(lua_State *L) {
	lua_sigar_fses_t *fses = (lua_sigar_fses_t *)luaL_checkudata(L, 1, "sigar_fses");

	lua_pushinteger(L, fses->fses.number);

	return 1;
}

static int lua_sigar_fs_gc(lua_State *L) {
	lua_sigar_fs_t *fs = (lua_sigar_fs_t *)luaL_checkudata(L, 1, "sigar_fs");

	lua_sigar_fses_free(L, fs->fses);

	return 0;
}

static int lua_sigar_fs_get_info(lua_State *L) {
	lua_sigar_fs_t *fs = (lua_sigar_fs_t *)luaL_checkudata(L, 1, "sigar_fs");

	lua_newtable(L);
#define DATA \
	(&(fs->fses->fses.data[fs->ndx]))

	LUA_EXPORT_STR(DATA, dir_name);
	LUA_EXPORT_STR(DATA, dev_name);
	LUA_EXPORT_STR(DATA, type_name);
	LUA_EXPORT_STR(DATA, sys_type_name);
	LUA_EXPORT_STR(DATA, options);
#if 0
	/* valgrind says: Conditional jump or move depends on uninitialised value
	 * looks like sigar isn't initializing it */
	LUA_EXPORT_INT(DATA, flags);
#endif
#undef DATA

	return 1;
}

static int lua_sigar_fs_get_usage(lua_State *L) {
	lua_sigar_fs_t *fs = (lua_sigar_fs_t *)luaL_checkudata(L, 1, "sigar_fs");
	const char *dir_name = fs->fses->fses.data[fs->ndx].dir_name;
	sigar_file_system_usage_t usage;
	int err;

	if (SIGAR_OK != (err = sigar_file_system_usage_get(fs->fses->sigar, dir_name, &usage))) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		return 2;
	}

	lua_newtable(L);
#define DATA \
	(&(usage))

	LUA_EXPORT_INT(DATA, total);
	LUA_EXPORT_INT(DATA, free);
	LUA_EXPORT_INT(DATA, used);
	LUA_EXPORT_INT(DATA, avail);
	LUA_EXPORT_INT(DATA, files);
	LUA_EXPORT_INT(DATA, free_files);
#undef DATA

	return 1;
}


static int lua_sigar_fses_get_fs(lua_State *L) {
	lua_sigar_fses_t *fses = (lua_sigar_fses_t *)luaL_checkudata(L, 1, "sigar_fses");
	lua_sigar_fs_t *fs;
	int ndx = luaL_checkint(L, 2);

	if (ndx - 1 < 0 || 
	    ndx - 1 >= fses->fses.number) {
		luaL_error(L, ".fses[%d] out of range: 1..%d", ndx, fses->fses.number);
	}

	fs = lua_newuserdata(L, sizeof(*fs));
	fs->ndx = ndx - 1;
	fs->fses = fses;
	fses->ref_count++;
	if (0 != luaL_newmetatable(L, "sigar_fs")) {
		lua_newtable(L);
		lua_pushcfunction(L, lua_sigar_fs_get_info);
		lua_setfield(L, -2, "info");
		lua_pushcfunction(L, lua_sigar_fs_get_usage);
		lua_setfield(L, -2, "usage");
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_fs_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);



	return 1;
}

int lua_sigar_fses_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	lua_sigar_fses_t *fses;

	fses = lua_newuserdata(L, sizeof(lua_sigar_fses_t));
	fses->sigar = s;
	sigar_file_system_list_get(s, &(fses->fses));
	fses->ref_count = 1;
	if (0 != luaL_newmetatable(L, "sigar_fses")) {
		lua_pushcfunction(L, lua_sigar_fses_len);
		lua_setfield(L, -2, "__len");
		lua_pushcfunction(L, lua_sigar_fses_get_fs);
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_fses_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}

