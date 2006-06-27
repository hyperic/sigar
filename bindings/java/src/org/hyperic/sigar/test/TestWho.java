package org.hyperic.sigar.test;

import org.hyperic.sigar.OperatingSystem;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.Who;

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
