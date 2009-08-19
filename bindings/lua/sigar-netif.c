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

/* network interfaces */
typedef struct {
	sigar_net_interface_list_t netifs;

	sigar_t *sigar;

	int ref_count;
} lua_sigar_netifs_t;

typedef struct {
	lua_sigar_netifs_t *netifs;

	int ndx;
} lua_sigar_netif_t;

static int lua_sigar_netifs_free(lua_State *L, lua_sigar_netifs_t *netifs) {
	if (--netifs->ref_count == 0) {
		sigar_net_interface_list_destroy(netifs->sigar, &(netifs->netifs));
	}

	return 0;
}

static int lua_sigar_netifs_gc(lua_State *L) {
	lua_sigar_netifs_t *netifs = (lua_sigar_netifs_t *)luaL_checkudata(L, 1, "sigar_netifs");

	lua_sigar_netifs_free(L, netifs);

	return 0;
}

static int lua_sigar_netifs_len(lua_State *L) {
	lua_sigar_netifs_t *netifs = (lua_sigar_netifs_t *)luaL_checkudata(L, 1, "sigar_netifs");

	lua_pushinteger(L, netifs->netifs.number);

	return 1;
}

static int lua_sigar_netif_gc(lua_State *L) {
	lua_sigar_netif_t *netif = (lua_sigar_netif_t *)luaL_checkudata(L, 1, "sigar_netif");

	lua_sigar_netifs_free(L, netif->netifs);

	return 0;
}

static int lua_sigar_netif_get_info(lua_State *L) {
	lua_sigar_netif_t *netif = (lua_sigar_netif_t *)luaL_checkudata(L, 1, "sigar_netif");
	int err;
	const char *if_name = netif->netifs->netifs.data[netif->ndx];
	sigar_net_interface_config_t usage;

	if (SIGAR_OK != (err = sigar_net_interface_config_get(netif->netifs->sigar, if_name, &usage))) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		return 2;
	}

	lua_newtable(L);
#define DATA \
	(&(usage))

	LUA_EXPORT_STR(DATA, name);
	LUA_EXPORT_STR(DATA, type);
	LUA_EXPORT_ADDRESS(DATA, hwaddr);
	LUA_EXPORT_ADDRESS(DATA, address);
	LUA_EXPORT_ADDRESS(DATA, destination);
	LUA_EXPORT_ADDRESS(DATA, broadcast);
	LUA_EXPORT_ADDRESS(DATA, netmask);
	LUA_EXPORT_INT(DATA, flags);
	LUA_EXPORT_INT(DATA, mtu);
	LUA_EXPORT_INT(DATA, metric);
#undef DATA

	return 1;

}

static int lua_sigar_netif_get_usage(lua_State *L) {
	lua_sigar_netif_t *netif = (lua_sigar_netif_t *)luaL_checkudata(L, 1, "sigar_netif");
	int err;
	const char *if_name = netif->netifs->netifs.data[netif->ndx];
	sigar_net_interface_stat_t usage;

	if (SIGAR_OK != (err = sigar_net_interface_stat_get(netif->netifs->sigar, if_name, &usage))) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		return 2;
	}

	lua_newtable(L);
#define DATA \
	(&(usage))

	LUA_EXPORT_INT(DATA, rx_packets);
	LUA_EXPORT_INT(DATA, rx_bytes);
	LUA_EXPORT_INT(DATA, rx_errors);
	LUA_EXPORT_INT(DATA, rx_overruns);
	LUA_EXPORT_INT(DATA, rx_dropped);
	LUA_EXPORT_INT(DATA, rx_frame);

	LUA_EXPORT_INT(DATA, tx_packets);
	LUA_EXPORT_INT(DATA, tx_bytes);
	LUA_EXPORT_INT(DATA, tx_errors);
	LUA_EXPORT_INT(DATA, tx_overruns);
	LUA_EXPORT_INT(DATA, tx_dropped);
	LUA_EXPORT_INT(DATA, tx_collisions);
	LUA_EXPORT_INT(DATA, tx_carrier);
	LUA_EXPORT_INT(DATA, speed);
#undef DATA

	return 1;
}


static int lua_sigar_netifs_get_netif(lua_State *L) {
	lua_sigar_netifs_t *netifs = (lua_sigar_netifs_t *)luaL_checkudata(L, 1, "sigar_netifs");
	lua_sigar_netif_t *netif;
	int ndx = luaL_checkint(L, 2);

	if (ndx - 1 < 0 || 
	    ndx - 1 >= netifs->netifs.number) {
		luaL_error(L, ".netifs[%d] out of range: 1..%d", ndx, netifs->netifs.number);
	}

	netif = lua_newuserdata(L, sizeof(*netif));
	netif->ndx = ndx - 1;
	netif->netifs = netifs;
	netifs->ref_count++;
	if (0 != luaL_newmetatable(L, "sigar_netif")) {
		lua_newtable(L);
		lua_pushcfunction(L, lua_sigar_netif_get_info);
		lua_setfield(L, -2, "info");
		lua_pushcfunction(L, lua_sigar_netif_get_usage);
		lua_setfield(L, -2, "usage");
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_netif_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);



	return 1;
}

int lua_sigar_netifs_get(lua_State *L) {
	sigar_t *s = *(sigar_t **)luaL_checkudata(L, 1, "sigar");
	lua_sigar_netifs_t *netifs;

	netifs = lua_newuserdata(L, sizeof(lua_sigar_netifs_t));
	netifs->sigar = s;
	sigar_net_interface_list_get(s, &(netifs->netifs));
	netifs->ref_count = 1;
	if (0 != luaL_newmetatable(L, "sigar_netifs")) {
		lua_pushcfunction(L, lua_sigar_netifs_len);
		lua_setfield(L, -2, "__len");
		lua_pushcfunction(L, lua_sigar_netifs_get_netif);
		lua_setfield(L, -2, "__index");
		lua_pushcfunction(L, lua_sigar_netifs_gc);
		lua_setfield(L, -2, "__gc");
	}
	lua_setmetatable(L, -2);

	return 1;
}


