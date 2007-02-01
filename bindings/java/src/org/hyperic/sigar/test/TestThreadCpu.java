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

import org.hyperic.sigar.CpuPerc;
import org.hyperic.sigar.CpuTimer;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.ThreadCpu;

public class TestThreadCpu extends SigarTestCase {

    public TestThreadCpu(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        ThreadCpu cpu;
        try {
            cpu = sigar.getThreadCpu();
        } catch (SigarNotImplementedException e) {
            return;
        }

        assertGtEqZeroTrace("User", cpu.getUser());

        assertGtEqZeroTrace("Sys", cpu.getSys());

        assertGtEqZeroTrace("Total", cpu.getTotal());

        CpuTimer timer = new CpuTimer(sigar);
        timer.start();

        for (int i=0; i<1000000; i++) {
            System.getProperty("java.home");
        }

        String sleepTime =
            System.getProperty("sigar.testThreadCpu.sleep");
        if (sleepTime != null) {
            Thread.sleep(Integer.parseInt(sleepTime) * 1000);
        }
        timer.stop();

        traceln("\nUsage...\n");

        assertGtEqZeroTrace("User", timer.getCpuUser());

        assertGtEqZeroTrace("Sys", timer.getCpuSys());

        assertGtEqZeroTrace("Total", timer.getCpuTotal());

        assertGtEqZeroTrace("Real Time", timer.getTotalTime());

        traceln("Cpu Percent=" + CpuPerc.format(timer.getCpuUsage()));
    }
}
