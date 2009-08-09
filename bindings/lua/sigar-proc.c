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
	sigar_proc_list_t procs;

	sigar_t *sigar;
} lua_sigar_procs_t;

static int lua_sigar_procs_gc(lua_State *L) {
	lua_sigar_procs_t *procs = (lua_sigar_procs_t *)luaL_checkudata(L, 1, "sigar_procs");

	sigar_proc_list_destroy(procs->sigar, &(procs->procs));

	return 0;
}

static int lua_sigar_procs_len(lua_State *L) {
	lua_sigar_procs_t *procs = (lua_sigar_procs_t *)luaL_checkudata(L, 1, "sigar_procs");

	lua_pushinteger(L, procs->procs.number);

	return 1;
}

static int lua_sigar_procs_get_pid(lua_State *L) {
	lua_sigar_procs_t *procs = (lua_sigar_procs_t *)luaL_checkudata(L, 1, "sigar_procs");
	int ndx = luaL_checkint(L, 2);

	if (ndx - 1 < 0 || 
	    ndx - 1 >= procs->procs.number) {
		luaL_error(L, ".procs[%d] out of range: 1..%d", ndx, procs->procs.number);
	}

	lua_pushinteger(L, procs->procs.data[ndx - 1]);

	return 1;
}

int lua_sigar_procs_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	lua_sigar_procs_t *procs;

	procs = lua_newuserdata(L, sizeof(lua_sigar_procs_t));
	procs->sigar = s;
	sigar_proc_list_get(s, &(procs->procs));
	if (0 != luaL_newmetatable(L, "sigar_procs")) {
		lua_pushcfunction(L, lua_sigar_procs_len);
		lua_setfield(L, -2, "__len");
		lua_pushcfunction(L, lua_sigar_procs_get_pid);
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_procs_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}

typedef struct {
	sigar_pid_t pid;

	sigar_t *sigar;
} lua_sigar_proc_t;

static int lua_sigar_proc_gc(lua_State *L) {
	lua_sigar_proc_t *proc = (lua_sigar_proc_t *)luaL_checkudata(L, 1, "sigar_proc");

	return 0;
}

static int lua_sigar_proc_get_mem(lua_State *L) {
	lua_sigar_proc_t *proc = (lua_sigar_proc_t *)luaL_checkudata(L, 1, "sigar_proc");
	sigar_proc_mem_t mem;
	int err;

	if (SIGAR_OK != (err = sigar_proc_mem_get(proc->sigar, proc->pid, &mem))) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		return 2;
	}

	lua_newtable(L);
#define DATA \
	(&(mem))

	LUA_EXPORT_INT(DATA, size);
	LUA_EXPORT_INT(DATA, resident);
	LUA_EXPORT_INT(DATA, share);
	LUA_EXPORT_INT(DATA, major_faults);
	LUA_EXPORT_INT(DATA, minor_faults);
	LUA_EXPORT_INT(DATA, page_faults);

#undef DATA

	return 1;
}

static int lua_sigar_proc_get_time(lua_State *L) {
	lua_sigar_proc_t *proc = (lua_sigar_proc_t *)luaL_checkudata(L, 1, "sigar_proc");
	sigar_proc_time_t t;
	int err;

	if (SIGAR_OK != (err = sigar_proc_time_get(proc->sigar, proc->pid, &t))) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		return 2;
		return 0;
	}

	lua_newtable(L);
#define DATA \
	(&(t))

	LUA_EXPORT_INT(DATA, start_time);
	LUA_EXPORT_INT(DATA, user);
	LUA_EXPORT_INT(DATA, sys);
	LUA_EXPORT_INT(DATA, total);

#undef DATA

	return 1;
}

static int lua_sigar_proc_get_state(lua_State *L) {
	lua_sigar_proc_t *proc = (lua_sigar_proc_t *)luaL_checkudata(L, 1, "sigar_proc");
	sigar_proc_state_t state;
	int err;

	if (SIGAR_OK != (err = sigar_proc_state_get(proc->sigar, proc->pid, &state))) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		return 2;
	}

	lua_newtable(L);
#define DATA \
	(&(state))

	LUA_EXPORT_STR(DATA, name);
	LUA_EXPORT_INT(DATA, ppid);
	LUA_EXPORT_INT(DATA, tty);
	LUA_EXPORT_INT(DATA, priority);
	LUA_EXPORT_INT(DATA, nice);
	LUA_EXPORT_INT(DATA, processor);
	LUA_EXPORT_INT(DATA, threads);

#undef DATA

	return 1;
}

static int lua_sigar_proc_get_exe(lua_State *L) {
	lua_sigar_proc_t *proc = (lua_sigar_proc_t *)luaL_checkudata(L, 1, "sigar_proc");
	sigar_proc_exe_t t;
	int err;

	if (SIGAR_OK != (err = sigar_proc_exe_get(proc->sigar, proc->pid, &t))) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		return 2;
	}

	lua_newtable(L);
#define DATA \
	(&(t))

	LUA_EXPORT_STR(DATA, name);
	LUA_EXPORT_STR(DATA, cwd);
	LUA_EXPORT_STR(DATA, root);

#undef DATA

	return 1;
}

int lua_sigar_proc_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	sigar_pid_t pid = luaL_checknumber(L, 2);
	lua_sigar_proc_t *proc;

	proc = lua_newuserdata(L, sizeof(lua_sigar_proc_t));
	proc->sigar = s;
	proc->pid   = pid;
	if (0 != luaL_newmetatable(L, "sigar_proc")) {
		lua_newtable(L);
		lua_pushcfunction(L, lua_sigar_proc_get_mem);
		lua_setfield(L, -2, "mem");
		lua_pushcfunction(L, lua_sigar_proc_get_time);
		lua_setfield(L, -2, "time");
		lua_pushcfunction(L, lua_sigar_proc_get_exe);
		lua_setfield(L, -2, "exe");
		lua_pushcfunction(L, lua_sigar_proc_get_state);
		lua_setfield(L, -2, "state");
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_proc_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}

int lua_sigar_pid_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");

	lua_pushnumber(L, sigar_pid_get(s));

	return 1;
}


