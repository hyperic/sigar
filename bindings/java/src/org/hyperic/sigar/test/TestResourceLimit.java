package org.hyperic.sigar.test;

import org.hyperic.sigar.ResourceLimit;
import org.hyperic.sigar.SigarException;

public class TestResourceLimit extends SigarTestCase {

    public TestResourceLimit(String name) {
        super(name);
    }

    public void testResourceLimit() throws SigarException {
        ResourceLimit limit = getSigar().getResourceLimit();
    }
}
