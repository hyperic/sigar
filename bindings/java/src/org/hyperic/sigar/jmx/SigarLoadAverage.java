/*
 * Copyright (C) [2004-2009], Hyperic, Inc.
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

import javax.management.AttributeNotFoundException;
import javax.management.MBeanException;
import javax.management.ReflectionException;

import org.hyperic.sigar.LoadAverage;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.SigarProxy;

public class SigarLoadAverage extends ReflectedMBean {

    private static final String MBEAN_TYPE = "LoadAverage";

    public SigarLoadAverage(SigarProxy sigar) {
        super(sigar, MBEAN_TYPE);
    }

    public Object getAttribute(String name)
        throws AttributeNotFoundException,
               MBeanException, ReflectionException {
        try {
            Object val =
                new LoadAverage(this.sigar.getLoadAverage()).toMap().get(name);
            if (val == null) {
                throw new AttributeNotFoundException(name);
            }
            return val;
        } catch (SigarNotImplementedException e) {
            return new Double(Sigar.FIELD_NOTIMPL);
        } catch (SigarException e) {
            throw new ReflectionException(e);
        }
    }
}
