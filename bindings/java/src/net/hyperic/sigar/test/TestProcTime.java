package net.hyperic.sigar.test;

import java.util.Date;

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
        traceln("StartDate=" + new Date(procTime.getStartTime()));
        //XXX
        //assertTrue(procTime.getStartTime() < System.currentTimeMillis());

        assertGtEqZeroTrace("User", procTime.getUser());

        assertGtEqZeroTrace("Sys", procTime.getSys());

        assertGtEqZeroTrace("Total", procTime.getTotal());
    }
}
