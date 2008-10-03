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

package org.hyperic.sigar.jmx;

import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.management.AttributeNotFoundException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.ReflectionException;

import org.hyperic.sigar.Sigar;

public class ReflectedMBean extends AbstractMBean {

    private Method method;
    private Map methods;
    private String type;

    protected String getType() {
        return this.type;
    }

    protected Class getMBeanClass() {
        try {
            return getMethod().getReturnType();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
    
    protected ReflectedMBean(Sigar sigar, String type) {
        super(sigar, CACHELESS);
        this.type = type;
    }

    public String getObjectName() {
        return
            SigarInvokerJMX.DOMAIN_NAME + ":" +
            MBEAN_ATTR_TYPE + "=" + getType();
    }

    protected Method getMethod() throws Exception {
        if (this.method == null) {
            String getName = "get" + getType();
            Class[] params = getMethodParamTypes();
            this.method =
                this.sigarImpl.getClass().getDeclaredMethod(getName,
                                                            params);
        }
        return this.method;
    }

    protected Class[] getMethodParamTypes() {
        return new Class[0];
    }

    private Object getReflectedAttribute(String name)
        throws Exception {

        Method method = getMethod();
        Object obj =
            method.invoke(this.sigarImpl,
                          getMethodParamTypes());
        Method attr =
            obj.getClass().getMethod("get" + name, new Class[0]);
        return attr.invoke(obj, new Object[0]);
    }

    public Object getAttribute(String name)
        throws AttributeNotFoundException,
               MBeanException, ReflectionException {
        try {
            return getReflectedAttribute(name);
        } catch (Exception e) {
            throw new ReflectionException(e);
        }
    }

    private Map getMethods() {
        if (this.methods != null) {
            return this.methods;
        }
        this.methods = new LinkedHashMap();
        Method[] methods = getMBeanClass().getDeclaredMethods();
        for (int i=0; i<methods.length; i++) {
            String name = methods[i].getName();
            if (!name.startsWith("get")) {
                continue;
            }
            name = name.substring(3);
            this.methods.put(name, methods[i]);
        }
        return this.methods;
    }

    protected MBeanAttributeInfo[] getAttributeInfo() {
        Map methods = getMethods();
        MBeanAttributeInfo[] attrs =
            new MBeanAttributeInfo[methods.size()];
        int i=0;
        for (Iterator it=methods.entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry entry = (Map.Entry)it.next();
            String name = (String)entry.getKey();
            Method method = (Method)entry.getValue();
            attrs[i++] =
                new MBeanAttributeInfo(name,
                                       method.getReturnType().getName(),
                                       name + " MBean",
                                       true,   // isReadable
                                       false,  // isWritable
                                       false); // isIs
        }        
        return attrs;
    }

    public MBeanInfo getMBeanInfo() {
        MBeanInfo info =
            new MBeanInfo(getMBeanClass().getName(),
                          "",
                          getAttributeInfo(),
                          null, //constructors
                          null, //operations
                          null); //notifications
        return info;
    }
}
