/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

package org.hyperic.sigar.test;

import org.hyperic.sigar.NetConnection;
import org.hyperic.sigar.NetFlags;
import org.hyperic.sigar.NetStat;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.SigarPermissionDeniedException;

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
        } catch (SigarPermissionDeniedException e) {
            return;
        }

        traceln("");
        assertGtEqZeroTrace("Outbound", netstat.getTcpOutboundTotal());
        assertGtEqZeroTrace("Inbound", netstat.getTcpInboundTotal());
        int[] states = netstat.getTcpStates();
        for (int i=0; i<NetFlags.TCP_UNKNOWN; i++) {
            assertGtEqZeroTrace(NetConnection.getStateString(i), states[i]);
        }
    }
}
