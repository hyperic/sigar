package net.hyperic.sigar;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class NetPortMap {

    private Sigar sigar;
    private Map inbound;
    private Map outbound;
    private int[] states;
    private int inboundTotal, outboundTotal;
    
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
        this.inboundTotal = this.outboundTotal = 0;
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
                this.outboundTotal++;
                ip = conn.getRemoteAddress();
                port = new Long(conn.getRemotePort());
                addresses = (Map)this.outbound.get(port);
                if (addresses == null) {
                    addresses = new HashMap();
                    this.outbound.put(port, addresses);
                }
            }
            else {
                this.inboundTotal++;
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

    public int getInboundConnectionsTotal() {
        return this.inboundTotal;
    }

    public int getOutboundConnectionsTotal() {
        return this.outboundTotal;
    }

    public int[] getStates() {
        return this.states;
    }

    private static void dumpConnections(Map map) {
        for (Iterator it = map.entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry entry = (Map.Entry)it.next();
            Long port = (Long)entry.getKey();
            Map addresses = (Map)entry.getValue();
            System.out.println(port + "=" + addresses);
        }
    }
    
    public static void main(String[] args) throws Exception {
        NetPortMap map = new NetPortMap();
        map.stat();

        System.out.println(map.inboundTotal +
                           " Inbound Connections...");
        dumpConnections(map.getInboundConnections());

        System.out.println("\n" + map.outboundTotal +
                           " Outbound Connections...");
        dumpConnections(map.getOutboundConnections());

        System.out.println("\nStates...");
        int[] states = map.getStates();
        for (int i=1; i<states.length; i++) {
            System.out.println(NetConnection.getStateString(i) + "=" +
                               states[i]);
        }
    }
}
