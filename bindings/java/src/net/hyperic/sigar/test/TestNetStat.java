package net.hyperic.sigar.test;

import net.hyperic.sigar.NetConnection;
import net.hyperic.sigar.NetFlags;
import net.hyperic.sigar.NetStat;
import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarNotImplementedException;

public class TestNetStat extends SigarTestCase {
    
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
        
        assertGtEqZeroTrace("Outbound", netstat.getTcpOutboundTotal());
        assertGtEqZeroTrace("Inbound", netstat.getTcpInboundTotal());
        assertGtEqZeroTrace("Listen", netstat.getTcpListen());

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
