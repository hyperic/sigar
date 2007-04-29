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

import javax.management.Attribute;
import javax.management.AttributeNotFoundException;
import javax.management.MBeanConstructorInfo;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanParameterInfo;
import javax.management.MBeanServer;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.ReflectionException;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;

/**
 * <p>Registry of all Sigar MBeans. Can be used as a convenient way to invoke 
 * Sigar MBeans at a central point. This brings a bunch of advantages with 
 * it:</p>
 * 
 * <ul>
 * <li>This class can be instantiated and registered to the MBean server by 
 * 	simply calling {@link MBeanServer#createMBean(String, ObjectName)}, 
 *  resulting in the automatic creation of all known default Sigar MBeans such 
 *  as CPU and memory monitoring beans.</li>
 * <li>Any Sigar MBean spawned by an instance of this class will use the same
 * 	{@link org.hyperic.sigar.Sigar} instance, saving resources in the 
 *  process.</li>
 * <li>When this instance is deregistered from the MBean server, it will 
 * 	automatically deregister all instances it created, cleaning up behind 
 *  itself.</li>
 * </ul>
 * 
 * <p>So using this class to manage the Sigar MBeans requires one line of code 
 * for creation, registration and MBean spawning, and one line of code to shut 
 * it all down again.</p>
 * 
 * @author Bjoern Martin
 * @since 1.4 (2007-04)
 */
public class SigarRegistry extends AbstractMBean {

	private static final String MBEAN_TYPE = "SigarRegistry";

	private static final MBeanInfo MBEAN_INFO;
	private static final MBeanConstructorInfo MBEAN_CONSTR_DEFAULT;
	
	static {
		MBEAN_CONSTR_DEFAULT = new MBeanConstructorInfo(
				SigarRegistry.class.getName(),
				"Creates a new instance of this class. Will create the Sigar " +
				"instance this class uses when constructing other MBeans",
				new MBeanParameterInfo[0]);
		
		MBEAN_INFO = new MBeanInfo(
				SigarRegistry.class.getName(),
				"Sigar MBean registry. Provides a central point for creation " +
				"and destruction of Sigar MBeans. Any Sigar MBean created via " +
				"this instance will automatically be cleaned up when this " +
				"instance is deregistered from the MBean server.",
				null /*new MBeanAttributeInfo[0]*/,
				new MBeanConstructorInfo[]{
					MBEAN_CONSTR_DEFAULT},
					null, null);
	}

	private final String objectName;

	private final ArrayList managedBeans;
	
	/**
	 * Creates a new instance of this class. Will create the Sigar instance this 
	 * class uses when constructing other MBeans.
	 */
	public SigarRegistry () {
		super(new Sigar(), CACHELESS);
		this.objectName = SigarInvokerJMX.DOMAIN_NAME + ":" 
				+ MBEAN_ATTR_TYPE + "=" + MBEAN_TYPE;
		this.managedBeans = new ArrayList();
	}
	
	/*
	 * @see AbstractMBean#getObjectName()
	 */
	public String getObjectName() {
		return this.objectName;
	}
	
	/* (non-Javadoc)
	 * @see javax.management.DynamicMBean#getMBeanInfo()
	 */
	public MBeanInfo getMBeanInfo() {
		return MBEAN_INFO;
	}

	/* (non-Javadoc)
	 * @see javax.management.DynamicMBean#getAttribute(java.lang.String)
	 */
	public Object getAttribute(String attr) throws AttributeNotFoundException {
		throw new AttributeNotFoundException(attr);
	}

	/* (non-Javadoc)
	 * @see javax.management.DynamicMBean#setAttribute(javax.management.Attribute)
	 */
	public void setAttribute(Attribute attr) throws AttributeNotFoundException {
		throw new AttributeNotFoundException(attr.getName());
	}

	/* (non-Javadoc)
	 * @see javax.management.DynamicMBean#invoke(java.lang.String, java.lang.Object[], java.lang.String[])
	 */
	public Object invoke(String action, Object[] params, String[] signatures)
			throws MBeanException, ReflectionException {
		throw new ReflectionException(new NoSuchMethodException(action), action);
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
		
		// get CPUs
		registerCpuBeans();
		
		// get memory
		registerMemoryBeans();
	}

	/**
	 * Registers MBeans for the Sigar types <code>Cpu</code>, <code>CpuPerc</code>
	 *  and <code>CpuInfo</code>. One instance will be registered for each CPU 
	 *  (core?) found.
	 */
	private void registerCpuBeans() {
		ObjectInstance nextRegistered = null;
		
		try {
			final int cpuCount = sigar.getCpuInfoList().length;
			for (int i = 0; i < cpuCount; i++) {
				SigarCpu nextCpu = new SigarCpu(sigarImpl, i);
				SigarCpuPerc nextCpuPerc = new SigarCpuPerc(sigarImpl, i);
				SigarCpuInfo nextCpuInfo = new SigarCpuInfo(sigarImpl, i);
				try {
					if (!mbeanServer.isRegistered(new ObjectName(nextCpu.getObjectName())))
						nextRegistered = mbeanServer.registerMBean(nextCpu, null);
				} catch (Exception e) { // ignore
				}
				// add MBean to set of managed beans
				managedBeans.add(nextRegistered.getObjectName());
				try {
					if (!mbeanServer.isRegistered(new ObjectName(nextCpuPerc.getObjectName())))
						nextRegistered = mbeanServer.registerMBean(nextCpuPerc, null);
				} catch (Exception e) { // ignore
				}
				// add MBean to set of managed beans
				managedBeans.add(nextRegistered.getObjectName());
				try {
					if (!mbeanServer.isRegistered(new ObjectName(nextCpuInfo.getObjectName())))
						nextRegistered = mbeanServer.registerMBean(nextCpuInfo, null);
				} catch (Exception e) { // ignore
				}
				// add MBean to set of managed beans
				managedBeans.add(nextRegistered.getObjectName());
			}
			
		} catch (SigarException e) {
			throw unexpectedError("CpuInfoList", e);
		}
	}
	
	/**
	 * Registers MBeans for the Sigar types <code>Mem</code> and <code>Swap</code>. 
	 */
	private void registerMemoryBeans() {
		
		ObjectInstance nextRegistered = null;
		
		SigarMem mem = new SigarMem(sigarImpl);
		
		try {
			if (!mbeanServer.isRegistered(new ObjectName(mem.getObjectName())))
				nextRegistered = mbeanServer.registerMBean(mem, null);
		} catch (Exception e) { // ignore
		}
		
		// add MBean to set of managed beans
		managedBeans.add(nextRegistered.getObjectName());
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
