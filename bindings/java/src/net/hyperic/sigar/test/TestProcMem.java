package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.ProcMem;

public class TestProcMem extends SigarTestCase {

    public TestProcMem(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcMem(getInvalidPid());
        } catch (SigarException e) {
        }

        ProcMem procMem = sigar.getProcMem(sigar.getPid());

        traceln("Size=" + Sigar.formatSize(procMem.getSize()));
        traceln("Vsize=" + Sigar.formatSize(procMem.getVsize()));
        traceln("Resident=" + Sigar.formatSize(procMem.getResident()));
        traceln("Share=" + Sigar.formatSize(procMem.getShare()));
        traceln("Rss=" + Sigar.formatSize(procMem.getRss()));
        assertTrue(procMem.getSize() > 0);
        // XXX vsize, resident, share, rss
    }
}
