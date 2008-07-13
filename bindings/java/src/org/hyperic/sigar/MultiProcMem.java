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

package org.hyperic.sigar;

import org.hyperic.sigar.ptql.ProcessFinder;

public class MultiProcMem extends ProcMem {

    static ProcMem get(Sigar sigar, String query)
        throws SigarException {

        ProcMem mem = new ProcMem();
        mem.share = Sigar.FIELD_NOTIMPL;

        long[] pids = ProcessFinder.find(sigar, query);

        for (int i=0; i<pids.length; i++) {
            ProcMem pmem;
            try {
                pmem = sigar.getProcMem(pids[i]);
            } catch (SigarException e) {
                //process may have gone away or EPERM
                continue;
            }
            mem.size     += pmem.size;
            mem.resident += pmem.resident;
            if (pmem.share != Sigar.FIELD_NOTIMPL) {
                mem.share += pmem.share;
            }
        }
        
        return mem;
    }
}
