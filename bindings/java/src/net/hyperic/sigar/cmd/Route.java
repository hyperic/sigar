package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.NetRoute;

public class Route {

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
                               route.getFlags() + "\t" +
                               route.getMtu() + "\t" +
                               route.getWindow() + "\t" +
                               route.getIrtt() + "\t" +
                               route.getIfname());
        }
    }
}
