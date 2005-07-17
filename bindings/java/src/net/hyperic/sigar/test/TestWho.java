package net.hyperic.sigar.test;

import net.hyperic.sigar.OperatingSystem;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.Who;

public class TestWho extends SigarTestCase {

    public TestWho(String name) {
        super(name);
    }

    public void testWho() throws SigarException {
        if (OperatingSystem.IS_WIN32) {
            return;
        }
        Who[] who = getSigar().getWhoList();
        assertTrue(who.length > 0);
    }
}
