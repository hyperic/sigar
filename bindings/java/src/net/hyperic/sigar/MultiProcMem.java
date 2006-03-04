package net.hyperic.sigar;

import net.hyperic.sigar.ptql.ProcessFinder;

public class MultiProcMem extends ProcMem {

    static ProcMem get(Sigar sigar, String query)
        throws SigarException {

        ProcMem mem = new ProcMem();

        long[] pids = ProcessFinder.find(sigar, query);

        for (int i=0; i<pids.length; i++) {
            ProcMem pmem = sigar.getProcMem(pids[i]);
            mem.size     += pmem.size;
            mem.resident += pmem.resident;
            mem.share    += pmem.share;
        }
        
        return mem;
    }
}
