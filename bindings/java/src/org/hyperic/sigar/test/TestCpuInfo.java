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
import org.hyperic.sigar.CpuInfo;

public class TestCpuInfo extends SigarTestCase {

    public TestCpuInfo(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        CpuInfo[] infos = sigar.getCpuInfoList();

        for (int i=0; i<infos.length; i++) {
            CpuInfo info = infos[i];

            traceln("num=" + i);
            traceln("vendor=" + info.getVendor());
            traceln("model=" + info.getModel());
            traceln("mhz=" + info.getMhz());
            traceln("cache size=" + info.getCacheSize());
            assertGtZeroTrace("totalSockets", info.getTotalSockets());
            assertGtZeroTrace("totalCores", info.getTotalCores());
            assertTrue(info.getTotalSockets() <= info.getTotalCores());
        }

        int mhz = infos[0].getMhz();
        int current = sigar.getCpuInfoList()[0].getMhz();
        assertEquals("Mhz=" + current + "/" + mhz, current, mhz);
    }
}
