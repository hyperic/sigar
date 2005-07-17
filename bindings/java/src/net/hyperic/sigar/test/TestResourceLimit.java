package net.hyperic.sigar.test;

import net.hyperic.sigar.ResourceLimit;
import net.hyperic.sigar.SigarException;

public class TestResourceLimit extends SigarTestCase {

    public TestResourceLimit(String name) {
        super(name);
    }

    public void testResourceLimit() throws SigarException {
        ResourceLimit limit = getSigar().getResourceLimit();
    }
}
