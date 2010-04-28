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

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.DynamicMBean;
import javax.management.MBeanException;
import javax.management.MBeanRegistration;
import javax.management.ReflectionException;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;

/**
 * Base class for all Sigar JMX MBeans. Provides a skeleton which handles
 * creation of the Sigar proxy instance and provides some convenience methods.
 * It also enforces usage of the {@link DynamicMBean} inferface while
 * implementing part of it, and it adds empty implementations for all methods of
 * the {@link MBeanRegistration} interface, allowing subclasses to only
 * implement subset of them.
 * 
 * @author Bjoern Martin
 * @since 1.5
 */
public abstract class AbstractMBean implements DynamicMBean {

    public static final String MBEAN_DOMAIN = SigarInvokerJMX.DOMAIN_NAME;
    public static final String MBEAN_ATTR_TYPE = SigarInvokerJMX.PROP_TYPE;

    /**
     * The Sigar proxy cache to be used in case the data does not have to be 
     * fetched during each call. The cache timeout is decided during 
     * construction. See {@link AbstractMBean#AbstractMBean(Sigar, short)} for 
     * details.
     * 
     * @see AbstractMBean#AbstractMBean(Sigar, short)
     */
    protected final SigarProxy sigar;

    /**
     * <p>Creates a new instance of this class. The SigarProxy instance is stored (and 
     * accessible) via the {@link #sigar} member.
     * 
     * @param sigar The SigarProxy instance to use. Must not be <code>null</code>
     */
    protected AbstractMBean(SigarProxy sigar) {
        this.sigar = sigar;
    }

    /**
     * Returns the object name the MBean is registered with within the
     * MBeanServer. May be <code>null</code> in case the instance is not
     * registered to an MBeanServer, but used standalone.
     * 
     * @return The object name or <code>null</code> if not registered to an
     *         MBeanServer
     */
    public abstract String getObjectName();

    /**
     * Returns a runtime exception for the type and SigarException specified.
     * 
     * @param type
     *            The type that was called
     * @param e
     *            The exception that was raised
     * @return A runtime exception encapsulating the information specified
     */
    protected RuntimeException unexpectedError(String type, SigarException e) {
        String msg = "Unexected error in Sigar.get" + type + ": "
                + e.getMessage();
        return new IllegalArgumentException(msg);
    }

    /**
     * Loops over all attributes and calls
     * {@link DynamicMBean#getAttribute(java.lang.String)} method for each
     * attribute sequentially. Any exception thrown by those methods are ignored
     * and simply cause the attribute not being added to the result.
     */
    public AttributeList getAttributes(String[] attrs) {
        final AttributeList result = new AttributeList();
        for (int i = 0; i < attrs.length; i++) {
            try {
                result.add(new Attribute(attrs[i], getAttribute(attrs[i])));
            } catch (AttributeNotFoundException e) {
                // ignore, as we cannot throw this exception
            } catch (MBeanException e) {
                // ignore, as we cannot throw this exception
            } catch (ReflectionException e) {
                // ignore, as we cannot throw this exception
            }
        }
        return result;
    }

    /**
     * Loops over all attributes and calls
     * {@link DynamicMBean#setAttribute(Attribute)} for each attribute
     * sequentially. Any exception thrown by those methods are ignored and
     * simply cause the attribute not being added to the result.
     */
    public AttributeList setAttributes(AttributeList attrs) {
        final AttributeList result = new AttributeList();
        for (int i = 0; i < attrs.size(); i++) {
            try {
                final Attribute next = (Attribute) attrs.get(i);
                setAttribute(next);
                result.add(next);
            } catch (AttributeNotFoundException e) {
                // ignore, as we cannot throw this exception
            }
        }
        return result;
    }

    public void setAttribute(Attribute attr) throws AttributeNotFoundException {
        throw new AttributeNotFoundException(attr.getName());
    }

    public Object invoke(String name, Object[] params, String[] signature)
        throws ReflectionException {

        throw new ReflectionException(new NoSuchMethodException(name),
                                      name);
    }
}
