package net.hyperic.sigar.test;

import java.util.ArrayList;

import net.hyperic.sigar.Sigar;

public class TestProcList extends SigarTestCase {

    public TestProcList(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        ArrayList traceList = new ArrayList();

        long[] pids = sigar.getProcList();

        assertTrue(pids.length > 1);

        long pid = sigar.getPid();
        boolean foundPid = false;

        //find our pid in the process list
        for (int i=0; i<pids.length; i++) {
            traceList.add(new Long(pids[i]));
            if (pid == pids[i]) {
                foundPid = true;
            }
        }

        traceln("pids=" + traceList);

        assertTrue(foundPid);
    }
}
