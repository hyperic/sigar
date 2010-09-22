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

import org.hyperic.sigar.SigarProxy;

public class ReflectedMBean extends AbstractMBean {

    private Map methods;
    private Map attrs = new HashMap();
    private String type;
    private String name;
    private SigarInvokerJMX invoker;

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

    private String getMBeanClassName() {
        String type = getType();
        final String list = "List";
        if (type.endsWith(list)) {
            type =
                type.substring(0, type.length() - list.length());
        }
        return "org.hyperic.sigar." + type;
    }

    private Class getMBeanClass() {
        try {
            return Class.forName(getMBeanClassName());
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    protected ReflectedMBean(SigarProxy sigar, String type) {
        super(sigar);
        this.type = type;
        this.name =
            MBEAN_DOMAIN + ":" +
            MBEAN_ATTR_TYPE + "=" + getType();
    }

    protected ReflectedMBean(SigarProxy sigar, String type, String arg) {
        this(sigar, type);
        this.name += ",Name=" + encode(arg);
    }

    private String encode(String arg) {
        return arg.replaceAll(":", "%3A");
    }

    public String getObjectName() {
        return this.name;
    }

    protected SigarInvokerJMX getInvoker() {
        if (this.invoker == null) {
            this.invoker =
                SigarInvokerJMX.getInstance(this.sigar, getObjectName());
            this.invoker.setType(getType());
        }
        return this.invoker;
    }
    
    public Object getAttribute(String name)
        throws AttributeNotFoundException,
               MBeanException, ReflectionException {
        Object val = this.attrs.get(name);
        if (val != null) {
            return val;
        }
        try {
            return getInvoker().invoke(name);
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
            new MBeanInfo(getClass().getName(),
                          getMBeanClassName(),
                          getAttributeInfo(),
                          null, //constructors
                          null, //operations
                          null); //notifications
        return info;
    }
}
