package org.hyperic.sigar.test;

import org.hyperic.sigar.NetRoute;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;

public class TestNetRoute extends SigarTestCase {

    public TestNetRoute(String name) {
        super(name);
    }

    public void testNetRoute() throws SigarException {
        NetRoute[] routes;
        try {
            routes = getSigar().getNetRouteList();
        } catch (SigarNotImplementedException e) {
            return;
        }
        assertTrue(routes.length > 0);
        for (int i=0; i<routes.length; i++) {
            NetRoute route = routes[i];
        }
    }
}
