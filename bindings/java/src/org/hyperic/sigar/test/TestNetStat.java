package org.hyperic.sigar.test;

import org.hyperic.sigar.NetConnection;
import org.hyperic.sigar.NetFlags;
import org.hyperic.sigar.NetStat;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarNotImplementedException;

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
        int[] states = netstat.getTcpStates();
        for (int i=0; i<NetFlags.TCP_UNKNOWN; i++) {
            assertGtEqZeroTrace(NetConnection.getStateString(i), states[i]);
        }

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
