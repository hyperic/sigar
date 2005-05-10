package net.hyperic.sigar.test;

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
    }
}
