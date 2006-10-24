/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

#if defined(WIN32) || defined(NETWARE)

#define WIN32_LEAN_AND_MEAN

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"
#include "sigar_util.h"

#include <errno.h>
#include <windows.h>
#ifdef NETWARE
#include <stdio.h>
#include <novsock2.h>
#include <ws2tcpip.h>
#include <monitor.h>
#endif

int sigar_get_iftype(const char *name, int *type, int *inst)
{
    if (strnEQ(name, "eth", IFTYPE_ETH)) {
        *type = IFTYPE_ETH;
    }
    else if (strnEQ(name, "lo", IFTYPE_LO)) {
        *type = IFTYPE_LO;
    }
    else {
        return EINVAL;
    }

    if (isdigit(*(name + *type))) {
        *inst = atoi(name + *type);
    }
    else {
        return EINVAL;
    }

    return SIGAR_OK;
}

#ifdef WIN32
int sigar_wsa_init(sigar_t *sigar)
{
    if (sigar->ws_version == 0) {
        WSADATA data;

        if (WSAStartup(MAKEWORD(2, 0), &data)) {
            sigar->ws_error = WSAGetLastError();
            WSACleanup();
            return sigar->ws_error;
        }

        sigar->ws_version = data.wVersion;
    }

    return SIGAR_OK;
}

#include <nb30.h>

static void hwaddr_lookup_netbios(sigar_net_interface_config_t *ifconfig,
                                  int num)
{
    NCB ncb;
    UCHAR rc;
    struct {
        ADAPTER_STATUS status;
        NAME_BUFFER name[30];
    } adapter;

    memset(&ncb, 0, sizeof(ncb));
    ncb.ncb_command = NCBRESET;
    ncb.ncb_lana_num = num;
    Netbios(&ncb);

    memset(&ncb, 0, sizeof(ncb));
    ncb.ncb_command = NCBASTAT;
    ncb.ncb_lana_num = num;

    /*
     * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/netbios/netbios_1l82.asp
     * mdsn docs claim this needs to be padded with spaces and
     * suggest the following silly code:
     * strcpy(ncb.ncb_callname,  "*               ");
     */
    ncb.ncb_callname[0] = '*';
    memset(&ncb.ncb_callname[1], ' ',
           sizeof(ncb.ncb_callname)-1);

    ncb.ncb_callname[sizeof(ncb.ncb_callname)-1] = '\0';

    ncb.ncb_buffer = (unsigned char *)&adapter;
    ncb.ncb_length = sizeof(adapter);
    if ((rc = Netbios(&ncb)) == 0) {
        sigar_net_address_mac_set(ifconfig->hwaddr,
                                  adapter.status.adapter_address,
                                  SIGAR_IFHWADDRLEN);
    }
    else {
        sigar_hwaddr_set_null(ifconfig);
    }
}

static void hwaddr_lookup(sigar_t *sigar,
                          sigar_net_interface_config_t *ifconfig,
                          int num)
{
    /* try IFMIB first, fallback on netbios for hwaddr */
    if (sigar_get_ifentry_config(sigar, ifconfig) != SIGAR_OK) {
        if (ifconfig->flags & SIGAR_IFF_LOOPBACK) {
            sigar_hwaddr_set_null(ifconfig);
        }
        else {
            hwaddr_lookup_netbios(ifconfig, num);
        }
    }
}

#else /* NETWARE */

static void hwaddr_lookup(sigar_t *sigar,
                          sigar_net_interface_config_t *ifconfig,
                          int num)
{
    uint8_t addr[SIGAR_IFHWADDRLEN];

    if (netware_net_macaddr(num+1, addr) == 0) {
        sigar_net_address_mac_set(ifconfig->hwaddr,
                                  addr,
                                  sizeof(addr));
    }
    else {
        sigar_hwaddr_set_null(ifconfig);
    }
}

#endif /* WIN32 */

static int sigar_ioctl_iflist(sigar_t *sigar,
                              SOCKET sock,
                              DWORD *bytes)
{
    return WSAIoctl(sock,
                    SIO_GET_INTERFACE_LIST,
                    NULL,
                    0,
                    (void *)sigar->ifconf_buf,
                    sigar->ifconf_len,
                    bytes,
                    NULL,
                    NULL);
}

static int get_iflist(sigar_t *sigar, DWORD *bytes)
{
    SOCKET sock = INVALID_SOCKET;
    int status, rc, limit;

#ifdef WIN32
    status = sigar_wsa_init(sigar);

    if (status != SIGAR_OK) {
        return status;
    }
    sock = WSASocket(PF_INET, SOCK_RAW, AF_INET, 0, 0, 0);
#else
    sock = socket(AF_INET, SOCK_DGRAM, 0);
#endif

    if (sock == INVALID_SOCKET) {
        return WSAGetLastError();
    }

    if (sigar->ifconf_len == 0) {
        sigar->ifconf_len = 8192;
        sigar->ifconf_buf = malloc(sigar->ifconf_len);
    }

    /*
     * XXX We can't tell ahead of time what buffer size is required
     * limit just incase.
     */
    for (limit=0; limit<100; limit++) {
        rc = sigar_ioctl_iflist(sigar, sock, bytes);
        if (rc && (WSAGetLastError() == WSAEFAULT)) {
            sigar->ifconf_len += (sizeof(INTERFACE_INFO) * 16);
            sigar->ifconf_buf = malloc(sigar->ifconf_len);
        }
    }

    status = rc ? WSAGetLastError() : SIGAR_OK;

    closesocket(sock);

    return status;
}

SIGAR_DECLARE(int)
sigar_net_interface_config_get(sigar_t *sigar,
                               const char *name,
                               sigar_net_interface_config_t *ifconfig)
{
    DWORD i, num, bytes;
    DWORD lo=0, eth=0;
    int status, type, inst;
    INTERFACE_INFO *if_info = NULL;
    u_long flags;

    /* win32 lacks socket ioctls to query given interface.
     * so we loop through the list to find our made up ifname.
     */
    status = get_iflist(sigar, &bytes);
    if (status != SIGAR_OK) {
        return status;
    }

    num = bytes / sizeof(INTERFACE_INFO);

    if ((status = sigar_get_iftype(name, &type, &inst)) != SIGAR_OK) {
        return status;
    }

    for (i=0; i<num ; i++) {
        if_info = ((INTERFACE_INFO *)sigar->ifconf_buf) + i;

        if (if_info->iiFlags & IFF_LOOPBACK) {
            if ((type == IFTYPE_LO) && (inst == lo)) {
                break;
            }
            ++lo;
        }
        else {
            if ((type == IFTYPE_ETH) && (inst == eth)) {
                break;
            }
            ++eth;
        }

        if_info = NULL;
    }

    if (!if_info) {
        return ENOENT;
    }

    SIGAR_ZERO(ifconfig);

    SIGAR_SSTRCPY(ifconfig->name, name);

#define if_s_addr(a) \
    ((struct sockaddr_in *)&a)->sin_addr.s_addr

    sigar_net_address_set(ifconfig->address,
                          if_s_addr(if_info->iiAddress));
    sigar_net_address_set(ifconfig->broadcast,
                          if_s_addr(if_info->iiBroadcastAddress));
    sigar_net_address_set(ifconfig->netmask,
                          if_s_addr(if_info->iiNetmask));

    flags = if_info->iiFlags;

    if (flags & IFF_UP) {
        ifconfig->flags |= SIGAR_IFF_UP|SIGAR_IFF_RUNNING;
    }
    if (flags & IFF_BROADCAST) {
        ifconfig->flags |= SIGAR_IFF_BROADCAST;
    }
    if (flags & IFF_LOOPBACK) {
        ifconfig->flags |= SIGAR_IFF_LOOPBACK;
        sigar_net_address_set(ifconfig->destination,
                              ifconfig->address.addr.in);
        sigar_net_address_set(ifconfig->broadcast, 0);
        SIGAR_SSTRCPY(ifconfig->type,
                      SIGAR_NIC_LOOPBACK);
    }
    else {
        SIGAR_SSTRCPY(ifconfig->type,
                      SIGAR_NIC_ETHERNET);
    }

    /* should be overridden w/ better description
     * using MIB_IFROW.bDescr when hwaddr is lookedup
     */
    SIGAR_SSTRCPY(ifconfig->description,
                  ifconfig->name);

    hwaddr_lookup(sigar, ifconfig, i);

    if (flags & IFF_POINTTOPOINT) {
        ifconfig->flags |= SIGAR_IFF_POINTOPOINT;
    }
    if (flags & IFF_MULTICAST) {
        ifconfig->flags |= SIGAR_IFF_MULTICAST;
    }

    return SIGAR_OK;
}

/*
 * win32 interface list does not include a name.
 * and the name from GetIfList() is the name of card
 * including vendor name, etc.  so we use 'eth' for ethernet
 * interfaces and 'lo' for loopback.
 */

#define ETH "eth"
#define LO  "lo"

SIGAR_DECLARE(int)
sigar_net_interface_list_get(sigar_t *sigar,
                             sigar_net_interface_list_t *iflist)
{
    char eth[56], lo[56];
    int ethcnt=0, locnt=0;
    DWORD i, num, bytes;
    int status;
    
    status = get_iflist(sigar, &bytes);
    if (status != SIGAR_OK) {
        return status;
    }

    num = bytes / sizeof(INTERFACE_INFO);

    iflist->number = 0;
    iflist->size = num;
    iflist->data =
        malloc(sizeof(*(iflist->data)) * iflist->size);

    for (i=0; i<num ; i++) {
        INTERFACE_INFO *if_info =
            ((INTERFACE_INFO *)sigar->ifconf_buf) + i;
        char *name;

        if (if_info->iiFlags & IFF_LOOPBACK) {
            sprintf(lo, LO "%d", locnt++);
            name = strdup(lo);
        }
        else {
            /* XXX: assuming ethernet here */
            sprintf(eth, ETH "%d", ethcnt++);
            name = strdup(eth);
        }

        iflist->data[iflist->number++] = name;
    }

    return SIGAR_OK;
}

#endif /* WIN32ISH */
