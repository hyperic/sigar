package net.hyperic.sigar.test;

import junit.framework.TestCase;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.ProcState;

public class TestProcState extends TestCase {

    public TestProcState(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        ProcState procState = sigar.getProcState(sigar.getPid());

        assertTrue(procState.getState() == 'R');

        assertTrue(procState.getName().indexOf("java") != -1);
    }
}
