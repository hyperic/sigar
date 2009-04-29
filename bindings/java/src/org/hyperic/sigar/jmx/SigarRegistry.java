/*
 * Copyright (C) [2004, 2005, 2006, 2007], Hyperic, Inc.
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

package org.hyperic.sigar.jmx;

import java.util.ArrayList;

import javax.management.AttributeNotFoundException;
import javax.management.MBeanInfo;
import javax.management.MBeanRegistration;
import javax.management.MBeanServer;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

import org.hyperic.sigar.CpuInfo;
import org.hyperic.sigar.FileSystem;
import org.hyperic.sigar.NetInterfaceConfig;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;

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
public class SigarRegistry extends AbstractMBean implements MBeanRegistration {

    private static final String MBEAN_TYPE = "SigarRegistry";

    private static final MBeanInfo MBEAN_INFO;

    static {
        MBEAN_INFO = new MBeanInfo(
                SigarRegistry.class.getName(),
                "Sigar MBean registry. Provides a central point for creation "
                        + "and destruction of Sigar MBeans. Any Sigar MBean created via "
                        + "this instance will automatically be cleaned up when this "
                        + "instance is deregistered from the MBean server.",
                null /*new MBeanAttributeInfo[0] */,
                null /*new MBeanConstructorInfo[0]*/,
                null /*new MBeanOperationInfo[0] */, 
                null /*new MBeanNotificationInfo[0]*/);
    }

    private final String objectName;

    private final ArrayList managedBeans;
    private MBeanServer mbeanServer;

    public SigarRegistry(SigarProxy sigar) {
        super(sigar);
        this.objectName = MBEAN_DOMAIN + ":" + MBEAN_ATTR_TYPE
                + "=" + MBEAN_TYPE;
        this.managedBeans = new ArrayList();
    }

    public String getObjectName() {
        return this.objectName;
    }

    public Object getAttribute(String attr) throws AttributeNotFoundException {
        throw new AttributeNotFoundException(attr);
    }

    public MBeanInfo getMBeanInfo() {
        return MBEAN_INFO;
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
        this.mbeanServer = server;                                                                            
        return new ObjectName(getObjectName());                                                               
    }

    /**
     * Registers the default set of Sigar MBeans. Before doing so, a super call 
     * is made to satisfy {@link AbstractMBean}.
     * 
     * @see AbstractMBean#postRegister(Boolean)
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
            throw unexpectedError("CpuInfoList", e);            
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
            throw unexpectedError("FileSystemList", e);
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
            throw unexpectedError("NetInterfaceList", e);
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
     * instance. After doing so, a super call is made to satisfy {@link AbstractMBean}.
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
    }
}
