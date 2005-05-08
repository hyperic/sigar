package net.hyperic.sigar;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class NetPortMap {

    private Sigar sigar;
    private Map inbound;
    private Map outbound;
    private int[] states;

    public NetPortMap() {
        this.sigar = new Sigar();
    }

    public static class IpEntry {
        int count;

        IpEntry() {
            this.count = 0;
        }

        public String toString() {
            return "count:" + this.count;
        }
    }

    public void stat() throws SigarException {
        int flags =
            NetFlags.CONN_SERVER | NetFlags.CONN_CLIENT |
            NetFlags.CONN_TCP;
        stat(flags);
    }
    
    public void stat(int flags) throws SigarException {
        this.inbound = new HashMap();
        this.outbound = new HashMap();
        this.states = new int[NetFlags.TCP_UNKNOWN];
        for (int i=0; i<this.states.length; i++) {
            this.states[i] = 0;
        }

        NetConnection[] connections =
            this.sigar.getNetConnectionList(flags);

        for (int i=0; i<connections.length; i++) {
            NetConnection conn = connections[i];
            int state = conn.getState();

            this.states[state]++;

            //first pass, get listening port numbers
            if (state == NetFlags.TCP_LISTEN) {
                Long port = new Long(conn.getLocalPort());
                Map addresses = (Map)this.inbound.get(port);

                if (addresses == null) {
                    addresses = new HashMap();
                    this.inbound.put(port, addresses);
                }
            }
        }

        //second pass, get addresses connected to listening ports
        for (int i=0; i<connections.length; i++) {
            NetConnection conn = connections[i];

            if (conn.getState() == NetFlags.TCP_LISTEN) {
                continue;
            }

            Long port = new Long(conn.getLocalPort());

            Map addresses = (Map)this.inbound.get(port);
            String ip = conn.getRemoteAddress();

            if (addresses == null) {
                ip = conn.getLocalAddress();
                String key =
                    conn.getRemoteAddress() + ":" + conn.getRemotePort();
                addresses = (Map)this.outbound.get(key);
                if (addresses == null) {
                    addresses = new HashMap();
                    this.outbound.put(key, addresses);
                }
            }

            IpEntry entry = (IpEntry)addresses.get(ip);
            if (entry == null) {
                entry = new IpEntry();
                addresses.put(ip, entry);
            }
            entry.count++;
        }
    }
    
    /**
     * Map listening tcp ports to connected remote addresses.
     * key == Listening tcp port on the local machine.
     * value == List of connected remote addresses.
     */
    public Map getInboundConnections() {
        return this.inbound;
    }

    public Map getOutboundConnections() {
        return this.outbound;
    }

    public int[] getStates() {
        return this.states;
    }

    public static void main(String[] args) throws Exception {
        NetPortMap map = new NetPortMap();
        map.stat();

        System.out.println("Inbound Connections...");
        Map ports = map.getInboundConnections();

        for (Iterator it = ports.entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry entry = (Map.Entry)it.next();
            Long port = (Long)entry.getKey();
            Map addresses = (Map)entry.getValue();
            System.out.println(port + "=" + addresses);
        }

        System.out.println("\nOutbound Connections...");
        Map outbound = map.getOutboundConnections();

        for (Iterator it = outbound.entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry entry = (Map.Entry)it.next();
            String server = (String)entry.getKey();
            Map addresses = (Map)entry.getValue();
            System.out.println(server + "=" + addresses);
        }

        System.out.println("\nStates...");
        int[] states = map.getStates();
        for (int i=1; i<states.length; i++) {
            System.out.println(NetConnection.getStateString(i) + "=" +
                               states[i]);
        }
    }
}
