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

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.NetInterfaceConfig;
import org.hyperic.sigar.NetInterfaceStat;
import org.hyperic.sigar.NetFlags;

public class TestNetIf extends SigarTestCase {

    public TestNetIf(String name) {
        super(name);
    }

    private void getNetIflist(Sigar sigar, boolean getStats) throws Exception {
        String[] ifNames = sigar.getNetInterfaceList();

        for (int i=0; i<ifNames.length; i++) {
            String name = ifNames[i];
            NetInterfaceConfig ifconfig =
                sigar.getNetInterfaceConfig(name);

            traceln("name=" + name);

            assertTrueTrace("Address", ifconfig.getAddress());
            assertTrueTrace("Netmask", ifconfig.getNetmask());

            if (!getStats) {
                continue;
            }

            if ((ifconfig.getFlags() & NetFlags.IFF_UP) <= 0) {
                traceln("!IFF_UP...skipping getNetInterfaceStat");
                continue;
            }

            try {
                NetInterfaceStat ifstat = sigar.getNetInterfaceStat(name);
                assertGtEqZeroTrace("RxPackets", ifstat.getRxPackets());
                assertGtEqZeroTrace("TxPackets", ifstat.getTxPackets());
                traceMethods(ifstat);
            } catch (SigarNotImplementedException e) {
                //ok
            } catch (SigarException e) {
                if (name.indexOf(':') == -1) {
                    fail("getNetInterfaceStat(" + name + "): " +
                         e.getMessage());
                } //else alias may not have metrics
            }
        }
    }

    private void getGarbage(Sigar sigar) {
        //test bogus arg results in exception (and not a segfault)
        try {
            traceln("testing bogus getNetInterfaceStat");
            sigar.getNetInterfaceStat("were switching to night vision");
            fail("switched to night vision");
        } catch (SigarException e) {
            //expected
        }

        //test bogus arg results in exception (and not a segfault)
        try {
            traceln("testing bogus getNetInterfaceConfig");
            sigar.getNetInterfaceConfig("happy meal");
            fail("unexpected treat in happy meal");
        } catch (SigarException e) {
            //expected
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        /* call twice to make sure caching works */
        getNetIflist(sigar, false);
        getNetIflist(sigar, false);
        getNetIflist(sigar, true);
        traceln("Default IP=" +
                sigar.getNetInterfaceConfig().getAddress());

        getGarbage(sigar);
    }
}
