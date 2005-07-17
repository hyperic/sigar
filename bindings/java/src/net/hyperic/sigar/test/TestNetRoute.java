package net.hyperic.sigar.test;

import net.hyperic.sigar.NetRoute;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarNotImplementedException;

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
