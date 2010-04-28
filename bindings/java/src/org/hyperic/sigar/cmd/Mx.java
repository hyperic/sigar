/*
 * Copyright (c) 2008-2009 Hyperic, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/* Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
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

package org.hyperic.sigar.cmd;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.jmx.SigarProcess;

public class Mx extends SigarCommandBase {

    private ObjectName registryName;

    public Mx(Shell shell) {
        super(shell);
    }

    public Mx() {
        super();
    }
    //java -Dcom.sun.management.jmxremote -jar sigar.jar
    public String getUsageShort() {
        return "Register MBeans for use via jconsole, etc.";
    }

    protected boolean validateArgs(String[] args) {
        return args.length <= 1;
    }

    public static MBeanServer getMBeanServer()
        throws SigarException {
        List servers = 
            MBeanServerFactory.findMBeanServer(null);
    
        if (servers.size() == 0) {
            throw new SigarException("No MBeanServers available");
        }
        else {
            return (MBeanServer)servers.get(0);
        }
    }

    private void register(MBeanServer server) throws SigarException {
        if (this.registryName != null) {
            return;
        }

        try {
            String name = org.hyperic.sigar.jmx.SigarRegistry.class.getName();
            this.registryName = server.createMBean(name, null).getObjectName();
            SigarProcess proc = new SigarProcess(this.sigar);
            ObjectName pname = new ObjectName(proc.getObjectName());
            if (!server.isRegistered(pname)) {
                server.registerMBean(proc, pname);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new SigarException(e.getMessage());
        }
    }

    private void jconsole() {
        String pid = String.valueOf(this.sigar.getPid());
        String[] argv = { "jconsole", pid };
        println("exec(jconsole, " + pid + ")");
        try {
            Process p = Runtime.getRuntime().exec(argv);
            p.waitFor();
            println("jconsole exited");
        } catch (Exception e) {
            println(e.getMessage());
        }        
    }

    public void output(String[] args) throws SigarException {
        MBeanServer server = getMBeanServer();
        register(server);
        boolean hasQuery = false;
        boolean launchJconsole = false;
        String query = "sigar:*";

        for (int i=0; i<args.length; i++) {
            String arg = args[i];
            if (arg.equals("-jconsole")) {
                launchJconsole = true;
            }
            else {
                query = arg;
                hasQuery = true;
            }
        }

        try {
            Set beans =
                server.queryNames(new ObjectName(query), null);
            println(beans.size() + " MBeans are registered...");
            for (Iterator it=beans.iterator(); it.hasNext();) {
                ObjectName name = (ObjectName)it.next(); 
                if (hasQuery) {
                    MBeanInfo info = server.getMBeanInfo(name);
                    MBeanAttributeInfo[] attrs = info.getAttributes();
                    for (int i=0; i<attrs.length; i++) {
                        String attr = attrs[i].getName();
                        Object val = server.getAttribute(name, attr);
                        println(name + ":" + attr + "=" + val);
                    }                    
                }
                else {
                    println(name.toString());
                }
            }
        } catch (Exception e) {
            throw new SigarException(e.getMessage());
        }
        if (launchJconsole) {
            flush();
            jconsole();
            try { //test unregisterMBean
                server.unregisterMBean(this.registryName);
                this.registryName = null;
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) throws Exception {
        new Mx().processCommand(args);
    }
}
