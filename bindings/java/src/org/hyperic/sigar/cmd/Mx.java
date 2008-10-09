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

import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.jmx.SigarRegistry;

public class Mx extends SigarCommandBase {

    private boolean isRegistered;

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
        if (isRegistered) {
            return;
        }
        SigarRegistry registry = new SigarRegistry();
        try {
            server.registerMBean(registry, null);
            isRegistered = true;
        } catch (Exception e) {
            throw new SigarException(e.getMessage());
        }
    }

    public void output(String[] args) throws SigarException {
        MBeanServer server = getMBeanServer();
        register(server);
        try {
            Set beans =
                server.queryNames(new ObjectName("sigar:*"), null);
            println(beans.size() + " MBeans are registered...");
            for (Iterator it=beans.iterator(); it.hasNext();) {
                println(it.next().toString());
            }
        } catch (Exception e) {
            throw new SigarException(e.getMessage());
        }
    }

    public static void main(String[] args) throws Exception {
        new Mx().processCommand(args);
    }
}
