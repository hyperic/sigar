/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
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

package org.hyperic.sigar.ptql;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarProxyCache;

public class ProcessFinder {

    private SigarProxy proxy;

    public ProcessFinder(SigarProxy proxy) {
        this.proxy = proxy;
        //getpid() cache to optimize queries on this process.
        this.proxy.getPid();
    }

    private Sigar getSigar() {
        return SigarProxyCache.getSigar(this.proxy);
    }

    public long findSingleProcess(String query)
        throws SigarException {

        ProcessQuery processQuery =
            ProcessQueryFactory.getInstance(query);
        return findSingleProcess(processQuery);
    }

    public long findSingleProcess(ProcessQuery query)
        throws SigarException {

        if (query instanceof SigarProcessQuery) {
            return ((SigarProcessQuery)query).findProcess(getSigar());
        }

        throw new SigarNotImplementedException();
    }

    public static long[] find(Sigar sigar, String query)
        throws SigarException {

        SigarProxy proxy = 
            SigarProxyCache.newInstance(sigar);

        return find(proxy, query);
    }

    public static long[] find(SigarProxy sigar, String query)
        throws SigarException {

        ProcessFinder finder = new ProcessFinder(sigar);

        return finder.find(ProcessQueryFactory.getInstance(query));
    }

    public long[] find(String query)
        throws SigarException {

        return find(ProcessQueryFactory.getInstance(query));
    }

    public long[] find(ProcessQuery query)
        throws SigarException {

        if (query instanceof SigarProcessQuery) {
            return ((SigarProcessQuery)query).findProcesses(getSigar());
        }

        throw new SigarNotImplementedException();
    }
}
    
