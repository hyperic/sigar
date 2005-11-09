package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarNotImplementedException;
import net.hyperic.sigar.Cpu;

public class TestCpu extends SigarTestCase {

    public TestCpu(String name) {
        super(name);
    }

    private void checkCpu(Cpu cpu) {
        traceln("User..." + cpu.getUser());
        assertTrue(cpu.getUser() >= 0);

        traceln("Sys...." + cpu.getSys());
        assertTrue(cpu.getSys() >= 0);

        traceln("Idle..." + cpu.getIdle());
        assertTrue(cpu.getIdle() >= 0);

        traceln("Wait..." + cpu.getWait());
        assertTrue(cpu.getWait() >= 0);

        traceln("Total.." + cpu.getTotal());
        assertTrue(cpu.getTotal() > 0);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();
        Cpu cpu = sigar.getCpu();

        traceln("getCpu:");
        checkCpu(cpu);

        try {
            Cpu[] cpuList = sigar.getCpuList();

            for (int i=0; i<cpuList.length; i++) {
                traceln("Cpu " + i + ":");
                checkCpu(cpuList[i]);
            }
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }
}
