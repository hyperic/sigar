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

typedef struct {
	sigar_cpu_list_t      data;
	sigar_cpu_info_list_t info;

	int ref_count;

	sigar_t *sigar;
} lua_sigar_cpus_t;

typedef struct {
	lua_sigar_cpus_t *cpus;

	int ndx;
} lua_sigar_cpu_t;

static int lua_sigar_cpus_free(lua_State *L, lua_sigar_cpus_t *cpus);

static int lua_sigar_cpu_gc(lua_State *L) {
	lua_sigar_cpu_t *cpu = (lua_sigar_cpu_t *)luaL_checkudata(L, 1, "sigar_cpu");

	lua_sigar_cpus_free(L, cpu->cpus);

	return 0;
}

static int lua_sigar_cpu_get_data(lua_State *L) {
	lua_sigar_cpu_t *cpu = (lua_sigar_cpu_t *)luaL_checkudata(L, 1, "sigar_cpu");

	lua_newtable(L);
#define DATA \
	(&(cpu->cpus->data.data[cpu->ndx]))

	LUA_EXPORT_INT(DATA, user);
	LUA_EXPORT_INT(DATA, sys);
	LUA_EXPORT_INT(DATA, idle);
	LUA_EXPORT_INT(DATA, nice);
	LUA_EXPORT_INT(DATA, wait);
	LUA_EXPORT_INT(DATA, irq);
	LUA_EXPORT_INT(DATA, soft_irq);
	LUA_EXPORT_INT(DATA, stolen);
	LUA_EXPORT_INT(DATA, total);

#undef DATA

	return 1;
}

static int lua_sigar_cpu_get_info(lua_State *L) {
	lua_sigar_cpu_t *cpu = (lua_sigar_cpu_t *)luaL_checkudata(L, 1, "sigar_cpu");

	lua_newtable(L);
#define DATA \
	(&(cpu->cpus->info.data[cpu->ndx]))

	LUA_EXPORT_STR(DATA, vendor);
	LUA_EXPORT_STR(DATA, model);
	LUA_EXPORT_INT(DATA, mhz);
	LUA_EXPORT_INT(DATA, total_sockets);
	LUA_EXPORT_INT(DATA, total_cores);
	LUA_EXPORT_INT(DATA, cores_per_socket);
	LUA_EXPORT_INT(DATA, cache_size);

#undef DATA

	return 1;
}


static int lua_sigar_cpus_get_cpu(lua_State *L) {
	lua_sigar_cpus_t *cpus = (lua_sigar_cpus_t *)luaL_checkudata(L, 1, "sigar_cpus");
	lua_sigar_cpu_t *cpu;
	int ndx = luaL_checkint(L, 2);

	if (ndx - 1 < 0 || 
	    ndx - 1 >= cpus->data.number) {
		luaL_error(L, ".cpu[%d] out of range: 1..%d", ndx, cpus->data.number);
	}

	cpu = lua_newuserdata(L, sizeof(lua_sigar_cpu_t));
	cpu->cpus = cpus;
	cpu->ndx = ndx - 1;
	cpus->ref_count++;

	if (0 != luaL_newmetatable(L, "sigar_cpu")) {
		lua_newtable(L);
		lua_pushcfunction(L, lua_sigar_cpu_get_data);
		lua_setfield(L, -2, "data");
		lua_pushcfunction(L, lua_sigar_cpu_get_info);
		lua_setfield(L, -2, "info");
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_cpu_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}

static int lua_sigar_cpus_free(lua_State *L, lua_sigar_cpus_t *cpus) {
	if (--cpus->ref_count == 0) {
		sigar_cpu_list_destroy(cpus->sigar, &(cpus->data));
		sigar_cpu_info_list_destroy(cpus->sigar, &(cpus->info));
	}

	return 0;
}

static int lua_sigar_cpus_gc(lua_State *L) {
	lua_sigar_cpus_t *cpus = (lua_sigar_cpus_t *)luaL_checkudata(L, 1, "sigar_cpus");

	lua_sigar_cpus_free(L, cpus);

	return 0;
}

static int lua_sigar_cpus_len(lua_State *L) {
	lua_sigar_cpus_t *cpus = (lua_sigar_cpus_t *)luaL_checkudata(L, 1, "sigar_cpus");

	lua_pushinteger(L, cpus->data.number);

	return 1;
}

int lua_sigar_cpus_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	lua_sigar_cpus_t *cpus;

	cpus = lua_newuserdata(L, sizeof(lua_sigar_cpus_t));
	cpus->sigar = s;
	sigar_cpu_list_get(s, &(cpus->data));
	sigar_cpu_info_list_get(s, &(cpus->info));
	cpus->ref_count = 1;
	if (0 != luaL_newmetatable(L, "sigar_cpus")) {
		lua_pushcfunction(L, lua_sigar_cpus_len);
		lua_setfield(L, -2, "__len");
		lua_pushcfunction(L, lua_sigar_cpus_get_cpu);
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_cpus_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}


