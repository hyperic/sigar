package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarNotImplementedException;
import net.hyperic.sigar.ThreadCpu;
import net.hyperic.sigar.ThreadCpuTime;

public class TestThreadCpu extends SigarTestCase {

    public TestThreadCpu(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        ThreadCpuTime cpu = new ThreadCpuTime(sigar);

        try {
            cpu.getCurrent();
        } catch (SigarNotImplementedException e) {
            return;
        }

        assertGtEqZeroTrace("User", cpu.getUser());

        assertGtEqZeroTrace("Sys", cpu.getSys());

        assertGtEqZeroTrace("Total", cpu.getTotal());

        for (int i=0; i<1000000; i++) {
            System.getProperty("java.home");
        }

        traceln("\nDiff...\n");

        ThreadCpu diff = cpu.getDiff();

        assertGtEqZeroTrace("User", diff.getUser());

        assertGtEqZeroTrace("Sys", diff.getSys());

        assertGtEqZeroTrace("Total", diff.getTotal());
    }
}
