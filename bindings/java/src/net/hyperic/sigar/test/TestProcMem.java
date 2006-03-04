package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.ProcMem;

public class TestProcMem extends SigarTestCase {

    public TestProcMem(String name) {
        super(name);
    }

    private void traceMem(Sigar sigar, long pid) throws Exception {
        ProcMem procMem;

        try {
            procMem = sigar.getProcMem(pid);
        } catch (SigarException e) {
            traceln("pid " + pid + ": " + e.getMessage());
            return;
        }

        traceln("Pid=" + pid);
        traceln("Size=" + Sigar.formatSize(procMem.getSize()));
        traceln("Resident=" + Sigar.formatSize(procMem.getResident()));
        traceln("Share=" + Sigar.formatSize(procMem.getShare()));
        traceln("MinorFaults=" + procMem.getMinorFaults());
        traceln("MajorFaults=" + procMem.getMajorFaults());
        traceln("PageFaults=" + procMem.getPageFaults());
        //assertTrue(procMem.getSize() > 0);
        // XXX vsize, resident, share, rss
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcMem(getInvalidPid());
        } catch (SigarException e) {
        }

        long[] pids = sigar.getProcList();
        for (int i=0; i<pids.length; i++) {
            traceMem(sigar, pids[i]);
        }
    }
}
