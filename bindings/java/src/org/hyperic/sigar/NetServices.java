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

package org.hyperic.sigar;

/**
 * @deprecated
 * @see org.hyperic.sigar.Sigar#getNetServicesName
 */
public class NetServices {

    private static NetServices instance;
    private Sigar sigar;

    private NetServices() {
        this.sigar = new Sigar();
    }

    protected void finalize() {
        this.sigar.close();
    }

    private static NetServices getInstance() {
        if (instance == null) {
            instance = new NetServices();
        }
        return instance;
    }

    private static String getServiceName(int protocol, long port) {
        return getInstance().sigar.getNetServicesName(protocol, port);
    }

    public static String getName(String protocol, long port) {
        if (protocol.equals("tcp")) {
            return getTcpName(port);
        }
        else if (protocol.equals("udp")) {
            return getUdpName(port);
        }
        else {
            return String.valueOf(port);
        }
    }

    public static String getTcpName(long port) {
        return getServiceName(NetFlags.CONN_TCP, port);
    }

    public static String getUdpName(long port) {
        return getServiceName(NetFlags.CONN_UDP, port);
    }
}
