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
import java.util.HashMap;
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
    private Map attrs = new HashMap();
    private String type;
    private Object[] args;
    private String name;

    protected String getType() {
        return this.type;
    }

    void setType(String type) {
        this.type = type;
    }

    //static attributes
    void putAttribute(String name, Object val) {
        this.attrs.put(name, val);
    }

    void putAttributes(Map attrs) {
        this.attrs.putAll(attrs);
    }

    void putAttributes(Object obj) {
        Method[] methods = obj.getClass().getDeclaredMethods();
        for (int i=0; i<methods.length; i++) {
            Method method = methods[i];
            if (method.getParameterTypes().length != 0) {
                continue;
            }
            String name = method.getName();
            if (!name.startsWith("get")) {
                continue;
            }
            name = name.substring(3);
            try {
                putAttribute(name,
                             method.invoke(obj, new Object[0]));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
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
        this.args = new Object[0];
        this.name =
            SigarInvokerJMX.DOMAIN_NAME + ":" +
            MBEAN_ATTR_TYPE + "=" + getType();
    }

    protected ReflectedMBean(Sigar sigar, String type, Object[] args) {
        this(sigar, type);
        this.args = args;
    }

    protected ReflectedMBean(Sigar sigar, String type, String arg) {
        this(sigar, type, new Object[] { arg });
        this.name += ",Name=" + encode(arg);
    }

    private String encode(String arg) {
        return arg.replaceAll(":", "%3A");
    }

    public String getObjectName() {
        return this.name;
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
        int len = this.args.length;
        Class[] types = new Class[len];
        for (int i=0; i<len; i++) {
            types[i] = this.args[i].getClass();
        }
        return types;
    }

    protected Object[] getMethodParams() {
        return this.args;
    }

    private Object getReflectedAttribute(String name)
        throws Exception {

        Method method = getMethod();
        Object obj =
            method.invoke(this.sigarImpl,
                          getMethodParams());
        Method attr =
            obj.getClass().getMethod("get" + name, new Class[0]);
        return attr.invoke(obj, new Object[0]);
    }

    public Object getAttribute(String name)
        throws AttributeNotFoundException,
               MBeanException, ReflectionException {
        Object val = this.attrs.get(name);
        if (val != null) {
            return val;
        }
        if (this.methods.get(name) == null) {
            throw new AttributeNotFoundException(name);
        }
        try {
            return getReflectedAttribute(name);
        } catch (Exception e) {
            e.printStackTrace();
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
            new MBeanAttributeInfo[methods.size() + this.attrs.size()];
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
        for (Iterator it=this.attrs.entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry entry = (Map.Entry)it.next();
            String name = (String)entry.getKey();
            Object obj = entry.getValue();
            attrs[i++] =
                new MBeanAttributeInfo(name,
                                       obj.getClass().getName(),
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
