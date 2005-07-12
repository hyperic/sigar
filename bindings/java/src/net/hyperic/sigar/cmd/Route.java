package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.NetFlags;
import net.hyperic.sigar.NetRoute;

public class Route {

    private static String flags(long flags) {
        StringBuffer sb = new StringBuffer();
        if ((flags & NetFlags.RTF_UP) != 0) {
            sb.append('U');
        }
        if ((flags & NetFlags.RTF_GATEWAY) != 0) {
            sb.append('G');
        }
        return sb.toString();
    }

    //netstat -r
    public static void main(String[] args) throws Exception {
        Sigar sigar = new Sigar();

        NetRoute[] routes = sigar.getNetRouteList();

        System.out.println("Destination\tGateway\tGenmask" +
                           "\tFlags\tMSS\tWindow\tirtt\tIface");

        for (int i=0; i<routes.length; i++) {
            NetRoute route = routes[i];

            System.out.println(route.getDestination() + "\t" +
                               route.getGateway() + "\t" +
                               route.getMask() + "\t" +
                               flags(route.getFlags()) + "\t" +
                               route.getMtu() + "\t" +
                               route.getWindow() + "\t" +
                               route.getIrtt() + "\t" +
                               route.getIfname());
        }
    }
}
