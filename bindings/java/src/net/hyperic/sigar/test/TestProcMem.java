package net.hyperic.sigar.test;

import junit.framework.TestCase;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.ProcMem;

public class TestProcMem extends TestCase {

    public TestProcMem(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        ProcMem procMem = sigar.getProcMem(sigar.getPid());

        assertTrue(procMem.getSize() > 0);
        // XXX vsize, resident, share, rss
    }
}
