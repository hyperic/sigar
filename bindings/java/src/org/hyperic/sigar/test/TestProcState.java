package org.hyperic.sigar.test;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.ProcState;

public class TestProcState extends SigarTestCase {

    public TestProcState(String name) {
        super(name);
    }

    private void traceState(Sigar sigar, long pid) {
        try {
            ProcState procState =
                sigar.getProcState(pid);
            traceln("[pid=" + pid + "] " + procState);
        } catch (SigarException e) {
            traceln("pid " + pid + ": " + e.getMessage());
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcState(getInvalidPid());
        } catch (SigarException e) {
        }

        ProcState procState = sigar.getProcState(sigar.getPid());
        traceState(sigar, sigar.getPid());

        char state = procState.getState();
        assertTrue((state == 'R') || (state == 'S'));
        assertTrue(procState.getName().indexOf("java") != -1);

        long[] pids = sigar.getProcList();
        for (int i=0; i<pids.length; i++) {
            traceState(sigar, pids[i]);
        }
    }
}
