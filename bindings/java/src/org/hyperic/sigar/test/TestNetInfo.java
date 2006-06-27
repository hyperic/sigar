package org.hyperic.sigar.test;

import org.hyperic.sigar.NetInfo;
import org.hyperic.sigar.SigarException;

public class TestNetInfo extends SigarTestCase {

    public TestNetInfo(String name) {
        super(name);
    }

    public void testNetInfo() throws SigarException {
        NetInfo info = getSigar().getNetInfo();
    }
}
