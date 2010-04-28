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

package org.hyperic.sigar.test;

import java.util.Iterator;
import java.util.Set;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.cmd.Mx;
import org.hyperic.sigar.jmx.SigarProcess;
import org.hyperic.sigar.jmx.SigarRegistry;

public class TestMx extends SigarTestCase {

    public TestMx(String name) {
        super(name);
    }

    public void testRegister() throws Exception {
        try {
            _testRegister();
        } catch (NoClassDefFoundError e) {
            //1.4 jre
            traceln(e + ", skipping");
        }
    }

    private void _testProcess(MBeanServer server) throws Exception {
        long[] pids = getSigar().getProcList();
        for (int i=0; i<pids.length; i++) {
            SigarProcess proc = new SigarProcess();
            proc.setPid(pids[i]);
            ObjectName name;
            try {
                name = new ObjectName(proc.getObjectName());
            } catch (SigarException e) {
                continue; //process may have gone away
            }
            if (server.isRegistered(name)) {
                continue;
            }
            server.registerMBean(proc, name);
        }
    }

    private void _testRegister() throws Exception {
        MBeanServer server;
        try {
            server = Mx.getMBeanServer();
        } catch (SigarException e) {
            traceln(e.getMessage());
            return;
        }

        SigarRegistry rgy = new SigarRegistry(getSigar());
        ObjectName name = new ObjectName(rgy.getObjectName());
        if (!server.isRegistered(name)) {
            server.registerMBean(rgy, name);
        }
        Set beans =
            server.queryNames(new ObjectName("sigar:*"), null);

        assertGtZeroTrace("beans.size", beans.size());

        for (Iterator it=beans.iterator(); it.hasNext();) {
            name = (ObjectName)it.next(); 
            MBeanInfo info = server.getMBeanInfo(name);
            MBeanAttributeInfo[] attrs = info.getAttributes();
            for (int k = 0; k < attrs.length; k++) {
                String attr = attrs[k].getName();
                Object o = server.getAttribute(name, attr);
                traceln(name + ":" + attr + "=" + o);
            }
        }

        _testProcess(server);
    }
}
