package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.Uptime;

public class TestUptime extends SigarTestCase {

    public TestUptime(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        Uptime uptime = sigar.getUptime();

        assertTrue(uptime.getUptime() > 0);
    }
}
