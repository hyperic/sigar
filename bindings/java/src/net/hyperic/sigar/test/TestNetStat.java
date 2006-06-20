package net.hyperic.sigar.test;

import net.hyperic.sigar.NetConnection;
import net.hyperic.sigar.NetFlags;
import net.hyperic.sigar.NetStat;
import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarNotImplementedException;

public class TestNetStat extends SigarTestCase {
    private static boolean printListeners =
        "true".equals(System.getProperty("sigar.netstat.listeners"));

    public TestNetStat(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();
        NetStat netstat;
        
        try {
            netstat = sigar.getNetStat();
        } catch (SigarNotImplementedException e) {
            return;
        }

        traceln("");
        assertGtEqZeroTrace("Outbound", netstat.getTcpOutboundTotal());
        assertGtEqZeroTrace("Inbound", netstat.getTcpInboundTotal());
        assertGtEqZeroTrace("Listen", netstat.getTcpListen());
        assertGtEqZeroTrace("Established", netstat.getTcpEstablished());

        if (!printListeners) {
            return;
        }

        int flags = NetFlags.CONN_SERVER | NetFlags.CONN_TCP;

        NetConnection[] connections =
            sigar.getNetConnectionList(flags);

        for (int i=0; i<connections.length; i++) {
            long port = connections[i].getLocalPort();
            traceln("Listen " +
                    sigar.getNetListenAddress(port) + ":" + port);
        }
    }
}
