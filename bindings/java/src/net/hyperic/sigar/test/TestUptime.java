package net.hyperic.sigar.test;

import junit.framework.TestCase;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.Uptime;

public class TestUptime extends TestCase {

    public TestUptime(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        Uptime uptime = sigar.getUptime();

        assertTrue(uptime.getUptime() > 0);
    }
}
