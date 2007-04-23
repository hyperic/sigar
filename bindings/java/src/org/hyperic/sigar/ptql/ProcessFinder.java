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
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarProxyCache;

public class ProcessFinder {

    private Sigar sigar;
    private ProcessQueryFactory qf;

    /**
     * @deprecated
     */
    public ProcessFinder(SigarProxy proxy) {
        this(SigarProxyCache.getSigar(proxy));
    }

    public ProcessFinder(Sigar sigar) {
        this.sigar = sigar;
        this.qf = ProcessQueryFactory.getInstance();
    }

    public long findSingleProcess(String query)
        throws SigarException {

        return findSingleProcess(this.qf.getQuery(query));
    }

    public long findSingleProcess(ProcessQuery query)
        throws SigarException {

        return query.findProcess(this.sigar);
    }

    public static long[] find(Sigar sigar, String query)
        throws SigarException {

        return new ProcessFinder(sigar).find(query);
    }

    public static long[] find(SigarProxy sigar, String query)
        throws SigarException {

        return new ProcessFinder(sigar).find(query);
    }

    public long[] find(String query)
        throws SigarException {

        return find(this.qf.getQuery(query));
    }

    public long[] find(ProcessQuery query)
        throws SigarException {

        return query.find(this.sigar);
    }
}
    
