package org.hyperic.sigar.test;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarNotImplementedException;

public class TestLoadAverage extends SigarTestCase {

    public TestLoadAverage(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            double[] loadavg = sigar.getLoadAverage();

            assertTrue(loadavg.length == 3);

            traceln("1min=" + loadavg[0]);
            traceln("5min=" + loadavg[1]);
            traceln("15min=" + loadavg[2]);
        } catch (SigarNotImplementedException e) {
            //win32
        }
    }
}
