package net.hyperic.sigar;

import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class NetPortMap {

    /**
     * Map listening tcp ports to connected remote addresses.
     * key == Listening tcp port on the local machine.
     * value == List of connected remote addresses.
     */
    public static Map getTcpConnections(Sigar sigar)
        throws SigarException {

        int flags =
            NetFlags.CONN_SERVER | NetFlags.CONN_TCP;

        Map map = new HashMap();

        NetConnection[] connections =
            sigar.getNetConnectionList(flags);

        //first pass, get listening port numbers
        for (int i=0; i<connections.length; i++) {
            NetConnection conn = connections[i];
            Long port = new Long(conn.getLocalPort());
            List addresses = (List)map.get(port);

            if (addresses == null) {
                addresses = new ArrayList();
                map.put(port, addresses);
            }
        }

        //second pass, get addresses connected to listening ports
        flags = NetFlags.CONN_CLIENT | NetFlags.CONN_TCP;
        connections = sigar.getNetConnectionList(flags);

        for (int i=0; i<connections.length; i++) {
            NetConnection conn = connections[i];
            Long port = new Long(conn.getLocalPort());
            List addresses = (List)map.get(port);

            if (addresses == null) {
                continue;
            }

            addresses.add(conn.getRemoteAddress());
        }

        return map;
    }

    public static void main(String[] args) throws Exception {
        Sigar sigar = new Sigar();
        Map ports = getTcpConnections(sigar);

        for (Iterator it = ports.entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry entry = (Map.Entry)it.next();
            Long port = (Long)entry.getKey();
            List addresses = (List)entry.getValue();
            System.out.println(port + "=" + addresses);
        } 
    }
}
