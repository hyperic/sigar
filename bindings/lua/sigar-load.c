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

int lua_sigar_load_get(lua_State *L) {
  int rc;
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	sigar_loadavg_t load;

	rc = sigar_loadavg_get(s, &load);

  if (rc != 0) {
    luaL_error(L, "sigar_loadavg_get error: %s", sigar_strerror(s, rc));
    return 0;
  }

	lua_newtable(L);

	lua_pushnumber(L, load.loadavg[0]);
	lua_setfield(L, -2, "5m");

	lua_pushnumber(L, load.loadavg[1]);
	lua_setfield(L, -2, "10m");

	lua_pushnumber(L, load.loadavg[2]);
	lua_setfield(L, -2, "15m");

	return 1;
}


