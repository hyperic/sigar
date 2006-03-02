package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarNotImplementedException;
import net.hyperic.sigar.Cpu;
import net.hyperic.sigar.CpuPerc;

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

    private static void printCpu(String prefix, CpuPerc cpu) {
        System.out.println(prefix +
                           CpuPerc.format(cpu.getUser()) + "\t" +
                           CpuPerc.format(cpu.getSys()) + "\t" +
                           CpuPerc.format(cpu.getWait()) + "\t" +
                           CpuPerc.format(cpu.getNice()) + "\t" +
                           CpuPerc.format(cpu.getIdle()) + "\t" +
                           CpuPerc.format(cpu.getCombined()));
    }

    public static void main(String[] args) throws Exception {
        final String HEADER =
            "   User\tSys\tWait\tNice\tIdle\tTotal";
        int interval = 1;
        if (args.length > 0) {
            interval = Integer.parseInt(args[0]);
        }
        int sleep = 1000 * 60 * interval;

        Sigar sigar = new Sigar();

        while (true) {
            System.out.println(HEADER);

            printCpu("   ", sigar.getCpuPerc());

            CpuPerc[] cpuList = sigar.getCpuPercList();

            for (int i=0; i<cpuList.length; i++) {
                printCpu(i+": ", cpuList[i]);
            }
            Thread.sleep(sleep);
        }
    }
}
