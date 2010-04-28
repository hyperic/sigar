/*
 * Copyright (c) 2007-2009 Hyperic, Inc.
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

package org.hyperic.sigar.jmx;

import java.util.ArrayList;

import javax.management.MBeanRegistration;
import javax.management.MBeanServer;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

import org.hyperic.sigar.CpuInfo;
import org.hyperic.sigar.FileSystem;
import org.hyperic.sigar.NetInterfaceConfig;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarProxyCache;

/**
 * <p>Registry of all Sigar MBeans. Can be used as a convenient way to invoke 
 * Sigar MBeans at a central point. This brings a bunch of advantages with 
 * it:</p>
 * 
 * <ul>
 * <li>This class can be instantiated and registered to the MBean server by 
 *     simply calling {@link MBeanServer#createMBean(String, ObjectName)}, 
 *  resulting in the automatic creation of all known default Sigar MBeans such 
 *  as CPU and memory monitoring beans.</li>
 * <li>Any Sigar MBean spawned by an instance of this class will use the same
 *     {@link org.hyperic.sigar.Sigar} instance, saving resources in the 
 *  process.</li>
 * <li>When this instance is deregistered from the MBean server, it will 
 *     automatically deregister all instances it created, cleaning up behind 
 *  itself.</li>
 * </ul>
 * 
 * <p>So using this class to manage the Sigar MBeans requires one line of code 
 * for creation, registration and MBean spawning, and one line of code to shut 
 * it all down again.</p>
 * 
 * @author Bjoern Martin
 * @since 1.5
 */
public class SigarRegistry implements MBeanRegistration, SigarRegistryMBean {
    public static final String MBEAN_DOMAIN = SigarInvokerJMX.DOMAIN_NAME;
    public static final String MBEAN_ATTR_TYPE = SigarInvokerJMX.PROP_TYPE;

    private static final String MBEAN_TYPE = "SigarRegistry";
    private static final int CACHE_EXPIRE = 60 * 1000;

    private Sigar sigarImpl;
    private SigarProxy sigar;
    private String objectName;

    private ArrayList managedBeans;
    private MBeanServer mbeanServer;

    public SigarRegistry() {}

    public SigarRegistry(SigarProxy sigar) {
        init(sigar);
    }

    private void init(SigarProxy sigar) {
        this.sigar = sigar;
        this.objectName =
            MBEAN_DOMAIN + ":" + MBEAN_ATTR_TYPE + "=" + MBEAN_TYPE;
        this.managedBeans = new ArrayList();        
    }

    public String getObjectName() {
        return this.objectName;
    }

    private void registerMBean(AbstractMBean mbean) {
        try {
            ObjectName name =
                new ObjectName(mbean.getObjectName());
            if (mbeanServer.isRegistered(name)) {
                return;
            }
            ObjectInstance instance =
                this.mbeanServer.registerMBean(mbean, name);
            this.managedBeans.add(instance.getObjectName());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // -------
    // Implementation of the MBeanRegistration interface
    // -------
    public ObjectName preRegister(MBeanServer server, ObjectName name)
        throws Exception {

        //no args constructor support
        if (this.sigar == null) {
            this.sigarImpl = new Sigar();
            init(SigarProxyCache.newInstance(this.sigarImpl, CACHE_EXPIRE));
        }

        this.mbeanServer = server;
        if (name == null) {
            return new ObjectName(getObjectName());                                                               
        }
        else {
            return name;
        }
    }

    /**
     * Registers the default set of Sigar MBeans.
     */
    public void postRegister(Boolean success) {
        ReflectedMBean mbean;

        if (!success.booleanValue())
            return;

        //CPU beans
        CpuInfo[] info;
        try {
            info = sigar.getCpuInfoList();            
        } catch (SigarException e) {
            info = new CpuInfo[0]; //XXX log
        }

        for (int i=0; i<info.length; i++) {
            String idx = String.valueOf(i);
            mbean =
                new ReflectedMBean(this.sigar, "CpuCore", idx);
            mbean.setType("CpuList");
            registerMBean(mbean);
            mbean =
                new ReflectedMBean(this.sigar, "CpuCoreUsage", idx);
            mbean.setType("CpuPercList");
            registerMBean(mbean);
        }

        mbean = new ReflectedMBean(this.sigar, "Cpu");
        mbean.putAttributes(info[0]);
        registerMBean(mbean);

        mbean = new ReflectedMBean(this.sigar, "CpuUsage");
        mbean.setType("CpuPerc");
        registerMBean(mbean);

        //FileSystem beans
        try {
            FileSystem[] fslist = this.sigar.getFileSystemList();
            for (int i=0; i<fslist.length; i++) {
                FileSystem fs = fslist[i];
                if (fs.getType() != FileSystem.TYPE_LOCAL_DISK) {
                    continue;
                }
                String name = fs.getDirName();
                mbean =
                    new ReflectedMBean(this.sigar, "FileSystem", name);
                mbean.setType(mbean.getType() + "Usage");
                mbean.putAttributes(fs);
                registerMBean(mbean);
            }
        } catch (SigarException e) {
            //XXX log
        }

        //NetInterface beans
        try {
            String[] ifnames = this.sigar.getNetInterfaceList();
            for (int i=0; i<ifnames.length; i++) {
                String name = ifnames[i];
                NetInterfaceConfig ifconfig =
                    this.sigar.getNetInterfaceConfig(name);
                try {
                    this.sigar.getNetInterfaceStat(name);
                } catch (SigarException e) {
                    continue;
                }
                mbean =
                    new ReflectedMBean(this.sigar, "NetInterface", name);
                mbean.setType(mbean.getType() + "Stat");
                mbean.putAttributes(ifconfig);
                registerMBean(mbean);
            }
        } catch (SigarException e) {
            //XXX log
        }

        //network info bean
        mbean = new ReflectedMBean(this.sigar, "NetInfo");
        try {
            mbean.putAttribute("FQDN", this.sigar.getFQDN());
        } catch (SigarException e) {
        }
        registerMBean(mbean);
        //physical memory bean
        registerMBean(new ReflectedMBean(this.sigar, "Mem"));
        //swap memory bean
        registerMBean(new ReflectedMBean(this.sigar, "Swap"));
        //load average bean
        registerMBean(new SigarLoadAverage(this.sigar));
        //global process stats
        registerMBean(new ReflectedMBean(this.sigar, "ProcStat"));
        //sigar version
        registerMBean(new ReflectedMBean(this.sigar, "SigarVersion"));
    }

    /**
     * Deregisters all Sigar MBeans that were created and registered using this 
     * instance.
     * @throws Exception 
     * 
     * @see AbstractMBean#preDeregister()
     */
    public void preDeregister() throws Exception {

        // count backwards to remove ONs immediately
        for (int i = managedBeans.size() - 1; i >= 0; i--) {
            ObjectName next = (ObjectName) managedBeans.remove(i);
            if (mbeanServer.isRegistered(next)) {
                try {
                    mbeanServer.unregisterMBean(next);
                } catch (Exception e) { // ignore
                }
            }
        }
    }

    public void postDeregister() {
        this.mbeanServer = null;
        if (this.sigarImpl != null) {
            this.sigarImpl.close();
            this.sigarImpl = null;
            this.sigar = null;
        }
    }
}
