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

import java.net.InetAddress;
import java.net.UnknownHostException;

public class NfsFileSystem extends FileSystem implements java.io.Serializable {

    private static final long serialVersionUID = 02242007L;

    private static final int NFS_PROGRAM = 100003;

    String hostname = null;

    public String getHostname() {
        if (this.hostname == null) {
            String dev = getDevName();
            int ix = dev.indexOf(":");
            if (ix != -1) {
                String host = dev.substring(0, ix);
                InetAddress addr;
                //try converting to ip in java land to take
                //advantage of InetAddress' lookup cache.
                try {
                    addr = InetAddress.getByName(host);
                    this.hostname = addr.getHostAddress();
                } catch (UnknownHostException e) {
                    this.hostname = host;
                }
            }
        }
        return this.hostname;
    }

    public boolean ping() {
        String hostname = getHostname();
        return
            (RPC.ping(hostname, RPC.TCP, NFS_PROGRAM, 2) == 0) ||
            (RPC.ping(hostname, RPC.UDP, NFS_PROGRAM, 2) == 0);
    }

    public String getUnreachableMessage() {
        return getDevName() + " nfs server unreachable";
    }

    public NfsUnreachableException getUnreachableException() {
        return new NfsUnreachableException(getUnreachableMessage());
    }

    public static void main(String[] args) throws Exception {
        Sigar.load();
        System.out.println(RPC.ping(args[0], "nfs"));
    }
}
