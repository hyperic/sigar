package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.Mem;

public class TestMem extends SigarTestCase {

    public TestMem(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        Mem mem = sigar.getMem();

        traceln("");

        assertGtZeroTrace("Total", mem.getTotal());

        assertGtZeroTrace("Used", mem.getUsed());

        assertGtZeroTrace("Free", mem.getFree());

        assertGtZeroTrace("Ram", mem.getRam());

        assertTrue((mem.getRam() % 8) == 0);
    }
}
