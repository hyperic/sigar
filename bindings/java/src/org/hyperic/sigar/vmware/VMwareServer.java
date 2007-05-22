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

package org.hyperic.sigar.vmware;

import java.util.List;

public class VMwareServer extends VMwareObject {
    native void destroy();

    private native void create();

    public native boolean connect(ConnectParams params)
        throws VMwareException;

    public native void disconnect();

    public native boolean isConnected();

    public native boolean isRegistered(String config)
        throws VMwareException;

    public native List getRegisteredVmNames()
        throws VMwareException;

    public native String getResource(String key)
        throws VMwareException;

    public native String exec(String xml)
        throws VMwareException;

    public VMwareServer() {
        create();
    }
}
