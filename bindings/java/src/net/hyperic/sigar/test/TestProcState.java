package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.ProcState;

public class TestProcState extends SigarTestCase {

    public TestProcState(String name) {
        super(name);
    }

    private void traceState(Sigar sigar, long pid) {
        try {
            ProcState procState =
                sigar.getProcState(pid);
            char state = procState.getState();
            traceln("[" + procState.getName() + "] " +
                    "pid=" + pid +
                    ",state=" + state +
                    ",threads=" + procState.getThreads() +
                    ",processor=" + procState.getProcessor() +
                    ",priority=" + procState.getPriority());
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
