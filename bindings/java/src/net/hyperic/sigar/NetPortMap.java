package net.hyperic.sigar;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class NetPortMap {

    private Sigar sigar;
    private Map tcpInbound;
    private Map tcpOutbound;
    private int[] tcpStates;
    private int tcpInboundTotal, tcpOutboundTotal;
    
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
        this.tcpInbound = new HashMap();
        this.tcpOutbound = new HashMap();
        this.tcpInboundTotal = this.tcpOutboundTotal = 0;
        this.tcpStates = new int[NetFlags.TCP_UNKNOWN];
        for (int i=0; i<this.tcpStates.length; i++) {
            this.tcpStates[i] = 0;
        }

        NetConnection[] connections =
            this.sigar.getNetConnectionList(flags);

        for (int i=0; i<connections.length; i++) {
            NetConnection conn = connections[i];
            int state = conn.getState();

            this.tcpStates[state]++;

            //first pass, get listening port numbers
            if (state == NetFlags.TCP_LISTEN) {
                Long port = new Long(conn.getLocalPort());
                Map addresses = (Map)this.tcpInbound.get(port);

                if (addresses == null) {
                    addresses = new HashMap();
                    this.tcpInbound.put(port, addresses);
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

            Map addresses = (Map)this.tcpInbound.get(port);
            String ip = conn.getRemoteAddress();

            if (addresses == null) {
                this.tcpOutboundTotal++;
                ip = conn.getRemoteAddress();
                port = new Long(conn.getRemotePort());
                addresses = (Map)this.tcpOutbound.get(port);
                if (addresses == null) {
                    addresses = new HashMap();
                    this.tcpOutbound.put(port, addresses);
                }
            }
            else {
                this.tcpInboundTotal++;
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
    public Map getTcpInboundConnections() {
        return this.tcpInbound;
    }

    public Map getTcpOutboundConnections() {
        return this.tcpOutbound;
    }

    public int getTcpInboundConnectionsTotal() {
        return this.tcpInboundTotal;
    }

    public int getTcpOutboundConnectionsTotal() {
        return this.tcpOutboundTotal;
    }

    public int[] getTcpStates() {
        return this.tcpStates;
    }

    //state counters
    public int getTcpEstablished() {
        return this.tcpStates[NetFlags.TCP_ESTABLISHED];
    }

    public int getTcpSynSent() {
        return this.tcpStates[NetFlags.TCP_SYN_SENT];
    }

    public int getTcpSynRecv() {
        return this.tcpStates[NetFlags.TCP_SYN_RECV];
    }

    public int getTcpFinWait1() {
        return this.tcpStates[NetFlags.TCP_FIN_WAIT1];
    }

    public int getTcpFinWait2() {
        return this.tcpStates[NetFlags.TCP_FIN_WAIT2];
    }

    public int getTcpTimeWait() {
        return this.tcpStates[NetFlags.TCP_TIME_WAIT];
    }

    public int getTcpClose() {
        return this.tcpStates[NetFlags.TCP_CLOSE];
    }

    public int getTcpCloseWait() {
        return this.tcpStates[NetFlags.TCP_CLOSE_WAIT];
    }

    public int getTcpLastAck() {
        return this.tcpStates[NetFlags.TCP_LAST_ACK];
    }

    public int getTcpListen() {
        return this.tcpStates[NetFlags.TCP_LISTEN];
    }

    public int getTcpClosing() {
        return this.tcpStates[NetFlags.TCP_CLOSING];
    }

    public int getTcpIdle() {
        return this.tcpStates[NetFlags.TCP_IDLE];
    }

    public int getTcpBound() {
        return this.tcpStates[NetFlags.TCP_BOUND];
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

        System.out.println(map.tcpInboundTotal +
                           " Inbound TCP Connections...");
        dumpConnections(map.getTcpInboundConnections());

        System.out.println("\n" + map.tcpOutboundTotal +
                           " Outbound TCP Connections...");
        dumpConnections(map.getTcpOutboundConnections());

        System.out.println("\nTCP States...");
        int[] states = map.getTcpStates();
        for (int i=1; i<states.length; i++) {
            System.out.println(NetConnection.getStateString(i) + "=" +
                               states[i]);
        }
    }
}
