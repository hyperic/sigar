package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.Swap;

public class TestSwap extends SigarTestCase {

    public TestSwap(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        Swap swap = sigar.getSwap();

        assertTrue(swap.getTotal() >= 0);

        assertTrue(swap.getUsed() >= 0);

        assertTrue(swap.getFree() >= 0);
    }
}
