package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.ProcTime;

public class TestProcTime extends SigarTestCase {

    public TestProcTime(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        try {
            sigar.getProcTime(getInvalidPid());
        } catch (SigarException e) {
        }

        ProcTime procTime = sigar.getProcTime(sigar.getPid());

        assertGtEqZeroTrace("StartTime", procTime.getStartTime());
        //XXX
        //assertTrue(procTime.getStartTime() < System.currentTimeMillis());

        assertGtEqZeroTrace("Utime", procTime.getUtime());

        assertGtEqZeroTrace("Stime", procTime.getStime());
    }
}
