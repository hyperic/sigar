package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.ProcStat;

public class TestProcStat extends SigarTestCase {

    public TestProcStat(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        ProcStat stat = sigar.getProcStat();

        long[] pids = sigar.getProcList();

        //oh-no a racing condition!
        //possible for this test to fail under normal 
        //conditions if the process table changes in between.
        //if this is a real problem, can just change to:
        //assertTrue(stat.getTotal() > 1);
        assertTrue(pids.length == stat.getTotal());
    }
}
