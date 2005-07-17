package net.hyperic.sigar.test;

import net.hyperic.sigar.NetInfo;
import net.hyperic.sigar.SigarException;

public class TestNetInfo extends SigarTestCase {

    public TestNetInfo(String name) {
        super(name);
    }

    public void testNetInfo() throws SigarException {
        NetInfo info = getSigar().getNetInfo();
    }
}
