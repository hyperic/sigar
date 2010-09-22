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
