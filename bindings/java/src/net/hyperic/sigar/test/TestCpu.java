package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.Cpu;

public class TestCpu extends SigarTestCase {

    public TestCpu(String name) {
        super(name);
    }

    private void checkCpu(Cpu cpu) {
        traceln("User=" + cpu.getUser());
        assertTrue(cpu.getUser() >= 0);

        traceln("Sys=" + cpu.getSys());
        assertTrue(cpu.getSys() >= 0);

        traceln("Total=" + cpu.getTotal());
        assertTrue(cpu.getTotal() >= 0);
    }

    public void testCreate() throws Exception {
        SigarProxy sigar = Sigar.getInstance(); //test reuse

        Cpu cpu = sigar.getCpu();

        traceln("");

        traceln("getCpu:");
        checkCpu(cpu);

        Cpu[] cpuList = sigar.getCpuList();

        for (int i=0; i<cpuList.length; i++) {
            traceln("Cpu " + i + ":");
            checkCpu(cpuList[i]);
        }
    }
}
