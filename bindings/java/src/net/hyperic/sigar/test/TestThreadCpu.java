package net.hyperic.sigar.test;

import net.hyperic.sigar.CpuTimer;
import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarNotImplementedException;
import net.hyperic.sigar.ThreadCpu;

public class TestThreadCpu extends SigarTestCase {

    public TestThreadCpu(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        ThreadCpu cpu;
        try {
            cpu = sigar.getThreadCpu();
        } catch (SigarNotImplementedException e) {
            return;
        }

        assertGtEqZeroTrace("User", cpu.getUser());

        assertGtEqZeroTrace("Sys", cpu.getSys());

        assertGtEqZeroTrace("Total", cpu.getTotal());

        CpuTimer timer = new CpuTimer(sigar);

        for (int i=0; i<1000000; i++) {
            System.getProperty("java.home");
        }

        traceln("\nDiff...\n");

        ThreadCpu diff = timer.getDiff();

        assertGtEqZeroTrace("User", diff.getUser());

        assertGtEqZeroTrace("Sys", diff.getSys());

        assertGtEqZeroTrace("Total", diff.getTotal());
    }
}
