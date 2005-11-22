package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.ProcState;

public class TestProcState extends SigarTestCase {

    public TestProcState(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcState(getInvalidPid());
        } catch (SigarException e) {
        }

        ProcState procState = sigar.getProcState(sigar.getPid());
        char state = procState.getState();
        traceln(procState.getName() + "=" + state);
        traceln("threads=" + procState.getThreads());
        traceln("priority=" + procState.getPriority());

        assertTrue((state == 'R') || (state == 'S'));

        assertTrue(procState.getName().indexOf("java") != -1);
    }
}
