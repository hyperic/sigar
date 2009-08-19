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

/* disks */
typedef struct {
	sigar_file_system_list_t disks;

	sigar_t *sigar;

	int ref_count;
} lua_sigar_disks_t;

typedef struct {
	char *dev_name;
	sigar_t *sigar;
} lua_sigar_disk_t;

static int lua_sigar_disks_free(lua_State *L, lua_sigar_disks_t *disks) {
	if (--disks->ref_count == 0) {
		sigar_file_system_list_destroy(disks->sigar, &(disks->disks));
	}

	return 0;
}

static int lua_sigar_disks_gc(lua_State *L) {
	lua_sigar_disks_t *disks = (lua_sigar_disks_t *)luaL_checkudata(L, 1, "sigar_disks");

	lua_sigar_disks_free(L, disks);

	return 0;
}

static int lua_sigar_disks_len(lua_State *L) {
	lua_sigar_disks_t *disks = (lua_sigar_disks_t *)luaL_checkudata(L, 1, "sigar_disks");

	lua_pushinteger(L, disks->disks.number);

	return 1;
}

static int lua_sigar_disk_gc(lua_State *L) {
	lua_sigar_disk_t *disk = (lua_sigar_disk_t *)luaL_checkudata(L, 1, "sigar_disk");

	free(disk->dev_name);

	return 0;
}

static int lua_sigar_disk_get_name(lua_State *L) {
	lua_sigar_disk_t *disk = (lua_sigar_disk_t *)luaL_checkudata(L, 1, "sigar_disk");

	lua_pushstring(L, disk->dev_name);

	return 1;
}

static int lua_sigar_disk_get_usage(lua_State *L) {
	lua_sigar_disk_t *disk = (lua_sigar_disk_t *)luaL_checkudata(L, 1, "sigar_disk");
	sigar_disk_usage_t usage;
	int err;

	if (SIGAR_OK != (err = sigar_disk_usage_get(disk->sigar, disk->dev_name, &usage))) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		return 2;
	}

	lua_newtable(L);
#define DATA \
	(&(usage))

	LUA_EXPORT_INT(DATA, reads);
	LUA_EXPORT_INT(DATA, writes);
	LUA_EXPORT_INT(DATA, read_bytes);
	LUA_EXPORT_INT(DATA, write_bytes);
	LUA_EXPORT_INT(DATA, rtime);
	LUA_EXPORT_INT(DATA, wtime);
	LUA_EXPORT_INT(DATA, qtime);
	LUA_EXPORT_INT(DATA, time);
	LUA_EXPORT_INT(DATA, snaptime);
	LUA_EXPORT_DOUBLE(DATA, service_time);
	LUA_EXPORT_DOUBLE(DATA, queue);
#undef DATA

	return 1;
}

static int lua_sigar_disk_set_metatable(lua_State *L, int ndx) {
	if (0 != luaL_newmetatable(L, "sigar_disk")) {
		lua_newtable(L);
		lua_pushcfunction(L, lua_sigar_disk_get_name);
		lua_setfield(L, -2, "name");
		lua_pushcfunction(L, lua_sigar_disk_get_usage);
		lua_setfield(L, -2, "usage");
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_disk_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, ndx - 1);

	return 0;
}

static int lua_sigar_disks_get_disk(lua_State *L) {
	lua_sigar_disks_t *disks = (lua_sigar_disks_t *)luaL_checkudata(L, 1, "sigar_disks");
	lua_sigar_disk_t *disk;
	int ndx = luaL_checkint(L, 2);

	if (ndx - 1 < 0 || 
	    ndx - 1 >= disks->disks.number) {
		luaL_error(L, ".disks[%d] out of range: 1..%d", ndx, disks->disks.number);
	}

	disk = lua_newuserdata(L, sizeof(*disk));
	disk->dev_name = strdup(disks->disks.data[ndx - 1].dir_name);
	disk->sigar = disks->sigar;
	lua_sigar_disk_set_metatable(L, -1);

	return 1;
}

int lua_sigar_disk_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	lua_sigar_disk_t *disk;
	const char *key = luaL_checkstring(L, 2);

	disk = lua_newuserdata(L, sizeof(*disk));
	disk->dev_name = strdup(key);
	disk->sigar = s;
	lua_sigar_disk_set_metatable(L, -1);

	return 1;
}


int lua_sigar_disks_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	lua_sigar_disks_t *disks;

	disks = lua_newuserdata(L, sizeof(lua_sigar_disks_t));
	disks->sigar = s;
	sigar_file_system_list_get(s, &(disks->disks));
	disks->ref_count = 1;
	if (0 != luaL_newmetatable(L, "sigar_disks")) {
		lua_pushcfunction(L, lua_sigar_disks_len);
		lua_setfield(L, -2, "__len");
		lua_pushcfunction(L, lua_sigar_disks_get_disk);
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_disks_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}

