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

        assertTrue(procMem.getSize() > 0);
        // XXX vsize, resident, share, rss
    }
}
