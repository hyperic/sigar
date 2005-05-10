package net.hyperic.sigar;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class NetStat {

    private Sigar sigar;
    private Map tcpInbound;
    private Map tcpOutbound;
    private int[] tcpStates;
    private int tcpInboundTotal, tcpOutboundTotal;

    public NetStat() {
        this.sigar = new Sigar();
    }

    public NetStat(Sigar sigar) throws SigarException {
        this.sigar = sigar;

        int flags =
            NetFlags.CONN_SERVER | NetFlags.CONN_CLIENT |
            NetFlags.CONN_TCP;
    
        stat(flags);
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
    public Map getTcpInbound() {
        return this.tcpInbound;
    }

    public Map getTcpOutbound() {
        return this.tcpOutbound;
    }

    public int getTcpInboundTotal() {
        return this.tcpInboundTotal;
    }

    public int getTcpOutboundTotal() {
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
        NetStat stat = new NetStat(new Sigar());

        System.out.println(stat.getTcpInboundTotal() +
                           " Inbound TCP Connections...");
        dumpConnections(stat.getTcpInbound());

        System.out.println("\n" + stat.getTcpOutboundTotal() +
                           " Outbound TCP Connections...");
        dumpConnections(stat.getTcpOutbound());

        System.out.println("\nTCP States...");
        int[] states = stat.getTcpStates();
        for (int i=1; i<states.length; i++) {
            System.out.println(NetConnection.getStateString(i) + "=" +
                               states[i]);
        }
    }
}
