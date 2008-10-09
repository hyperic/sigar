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
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.management.AttributeNotFoundException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanConstructorInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanParameterInfo;
import javax.management.MBeanServer;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

import org.hyperic.sigar.CpuInfo;
import org.hyperic.sigar.FileSystem;
import org.hyperic.sigar.NetInterfaceConfig;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarLoader;

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
public class SigarRegistry extends AbstractMBean {

    private static final String MBEAN_TYPE = "SigarRegistry";

    private static final Map VERSION_ATTRS = new LinkedHashMap();

    private static final MBeanInfo MBEAN_INFO;

    private static final MBeanConstructorInfo MBEAN_CONSTR_DEFAULT;

//    private static final MBeanOperationInfo MBEAN_OPER_LISTPROCESSES;

    static {
        MBEAN_CONSTR_DEFAULT = new MBeanConstructorInfo(
                SigarRegistry.class.getName(),
                "Creates a new instance of this class. Will create the Sigar "
                        + "instance this class uses when constructing other MBeans",
                new MBeanParameterInfo[0]);
//        MBEAN_OPER_LISTPROCESSES = new MBeanOperationInfo("listProcesses",
//                "Executes a query returning the process IDs of all processes " +
//                "found on the system.",
//                null /* new MBeanParameterInfo[0] */,
//                String.class.getName(), MBeanOperationInfo.INFO);

        MBEAN_INFO = new MBeanInfo(
                SigarRegistry.class.getName(),
                "Sigar MBean registry. Provides a central point for creation "
                        + "and destruction of Sigar MBeans. Any Sigar MBean created via "
                        + "this instance will automatically be cleaned up when this "
                        + "instance is deregistered from the MBean server.",
                getAttributeInfo(),
                new MBeanConstructorInfo[] { MBEAN_CONSTR_DEFAULT },
                null /*new MBeanOperationInfo[0] */, 
                null /*new MBeanNotificationInfo[0]*/);
    }

    private final String objectName;

    private final ArrayList managedBeans;

    private static MBeanAttributeInfo[] getAttributeInfo() {
        VERSION_ATTRS.put("JarVersion", Sigar.VERSION_STRING);
        VERSION_ATTRS.put("NativeVersion", Sigar.NATIVE_VERSION_STRING);
        VERSION_ATTRS.put("JarBuildDate", Sigar.BUILD_DATE);
        VERSION_ATTRS.put("NativeBuildDate", Sigar.NATIVE_BUILD_DATE);
        VERSION_ATTRS.put("JarSourceRevision", Sigar.SCM_REVISION);
        VERSION_ATTRS.put("NativeSourceRevision", Sigar.NATIVE_SCM_REVISION);
        VERSION_ATTRS.put("NativeLibraryName", SigarLoader.getNativeLibraryName());

        MBeanAttributeInfo[] attrs = new MBeanAttributeInfo[VERSION_ATTRS.size()];
        int i=0;
        for (Iterator it=VERSION_ATTRS.entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry entry = (Map.Entry)it.next();
            String name = (String)entry.getKey();
            attrs[i++] =
                new MBeanAttributeInfo(name,
                                       entry.getValue().getClass().getName(),
                                       name,
                                       true,   // isReadable
                                       false,  // isWritable
                                       false); // isIs
        }
        return attrs;
    }

    /**
     * Creates a new instance of this class. Will create the Sigar instance this 
     * class uses when constructing other MBeans.
     */
    public SigarRegistry() {
        super(new Sigar(), CACHELESS);
        this.objectName = SigarInvokerJMX.DOMAIN_NAME + ":" + MBEAN_ATTR_TYPE
                + "=" + MBEAN_TYPE;
        this.managedBeans = new ArrayList();
    }

    /* (non-Javadoc)
     * @see AbstractMBean#getObjectName()
     */
    public String getObjectName() {
        return this.objectName;
    }

/*  public String listProcesses() {
        try {
            final long start = System.currentTimeMillis();
            long[] ids = sigar.getProcList();
            StringBuffer procNames = new StringBuffer();
            for (int i = 0; i < ids.length; i++) {
                try {
                    procNames.append(ids[i] + ":" + sigar.getProcExe(ids[i]).getName()).append('\n');
                } catch (SigarException e) {
                    procNames.append(ids[i] + ":" + e.getMessage()).append('\n');
                }
            }
            
            final long end = System.currentTimeMillis();
            procNames.append("-- Took " + (end-start) + "ms");
            return procNames.toString();

        } catch (SigarException e) {
            throw unexpectedError("ProcList", e);
        }
    }
*/
    /* (non-Javadoc)
     * @see javax.management.DynamicMBean#getAttribute(java.lang.String)
     */
    public Object getAttribute(String attr) throws AttributeNotFoundException {
        Object obj = VERSION_ATTRS.get(attr);
        if (obj == null) {
            throw new AttributeNotFoundException(attr);
        }
        return obj;
    }

    /* (non-Javadoc)
     * @see javax.management.DynamicMBean#getMBeanInfo()
     */
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

    /**
     * Registers the default set of Sigar MBeans. Before doing so, a super call 
     * is made to satisfy {@link AbstractMBean}.
     * 
     * @see AbstractMBean#postRegister(Boolean)
     */
    public void postRegister(Boolean success) {

        super.postRegister(success);

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
            ReflectedMBean mbean =
                new ReflectedMBean(sigarImpl, "CpuCoreTime", idx);
            mbean.setType("CpuList");
            registerMBean(mbean);
            mbean =
                new ReflectedMBean(sigarImpl, "CpuCoreUsage", idx);
            mbean.setType("CpuPercList");
            registerMBean(mbean);
        }

        //FileSystem beans
        try {
            FileSystem[] fslist = sigarImpl.getFileSystemList();
            for (int i=0; i<fslist.length; i++) {
                FileSystem fs = fslist[i];
                if (fs.getType() != FileSystem.TYPE_LOCAL_DISK) {
                    continue;
                }
                String name = fs.getDirName();
                ReflectedMBean mbean =
                    new ReflectedMBean(sigarImpl, "FileSystem", name);
                mbean.setType(mbean.getType() + "Usage");
                mbean.putAttributes(fs);
                registerMBean(mbean);
            }
        } catch (SigarException e) {
            throw unexpectedError("FileSystemList", e);
        }

        //NetInterface beans
        try {
            String[] ifnames = sigarImpl.getNetInterfaceList();
            for (int i=0; i<ifnames.length; i++) {
                String name = ifnames[i];
                NetInterfaceConfig ifconfig =
                    this.sigar.getNetInterfaceConfig(name);
                try {
                    sigarImpl.getNetInterfaceStat(name);
                } catch (SigarException e) {
                    continue;
                }
                ReflectedMBean mbean =
                    new ReflectedMBean(sigarImpl, "NetInterface", name);
                mbean.setType(mbean.getType() + "Stat");
                mbean.putAttributes(ifconfig);
                registerMBean(mbean);
            }
        } catch (SigarException e) {
            throw unexpectedError("NetInterfaceList", e);
        }

        //network info bean
        ReflectedMBean mbean = new ReflectedMBean(sigarImpl, "NetInfo");
        try {
            mbean.putAttribute("FQDN", sigarImpl.getFQDN());
        } catch (SigarException e) {
        }
        registerMBean(mbean);
        //physical memory bean
        registerMBean(new ReflectedMBean(sigarImpl, "Mem"));
        //swap memory bean
        registerMBean(new ReflectedMBean(sigarImpl, "Swap"));
        //load average bean
        registerMBean(new SigarLoadAverage(sigarImpl));
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

        // do the super call
        super.preDeregister();
    }
}
