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

        assertGtEqZeroTrace("Total", swap.getTotal());

        assertGtEqZeroTrace("Used", swap.getUsed());

        assertGtEqZeroTrace("Free", swap.getFree());

        assertEqualsTrace("Total-Used==Free",
                          swap.getTotal() - swap.getUsed(),
                          swap.getFree());
    }
}
