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
import org.hyperic.sigar.Cpu;
import org.hyperic.sigar.CpuPerc;

public class TestCpu extends SigarTestCase {

    public TestCpu(String name) {
        super(name);
    }

    private void checkCpu(Cpu cpu) {
        traceln("User..." + cpu.getUser());
        assertTrue(cpu.getUser() >= 0);

        traceln("Sys...." + cpu.getSys());
        assertTrue(cpu.getSys() >= 0);

        traceln("Idle..." + cpu.getIdle());
        assertTrue(cpu.getIdle() >= 0);

        traceln("Wait..." + cpu.getWait());
        assertTrue(cpu.getWait() >= 0);

        traceln("Irq..." + cpu.getIrq());
        assertTrue(cpu.getIrq() >= 0);

        traceln("SIrq.." + cpu.getSoftIrq());
        assertTrue(cpu.getSoftIrq() >= 0);

        traceln("Stl..." + cpu.getStolen());
        assertTrue(cpu.getStolen() >= 0);

        traceln("Total.." + cpu.getTotal());
        assertTrue(cpu.getTotal() > 0);

        try {
            long current =
                getSigar().getProcState("$$").getProcessor();
            traceln("last run cpu=" + current);
        } catch (SigarException e) {
            e.printStackTrace();
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();
        Cpu cpu = sigar.getCpu();

        traceln("getCpu:");
        checkCpu(cpu);

        try {
            Cpu[] cpuList = sigar.getCpuList();

            for (int i=0; i<cpuList.length; i++) {
                traceln("Cpu " + i + ":");
                checkCpu(cpuList[i]);
            }
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }

    private static void printCpu(String prefix, CpuPerc cpu) {
        System.out.println(prefix +
                           CpuPerc.format(cpu.getUser()) + "\t" +
                           CpuPerc.format(cpu.getSys()) + "\t" +
                           CpuPerc.format(cpu.getWait()) + "\t" +
                           CpuPerc.format(cpu.getNice()) + "\t" +
                           CpuPerc.format(cpu.getIdle()) + "\t" +
                           CpuPerc.format(cpu.getCombined()));
    }

    public static void main(String[] args) throws Exception {
        final String HEADER =
            "   User\tSys\tWait\tNice\tIdle\tTotal";
        int interval = 1;
        if (args.length > 0) {
            interval = Integer.parseInt(args[0]);
        }
        int sleep = 1000 * 60 * interval;

        Sigar sigar = new Sigar();

        while (true) {
            System.out.println(HEADER);

            printCpu("   ", sigar.getCpuPerc());

            CpuPerc[] cpuList = sigar.getCpuPercList();

            for (int i=0; i<cpuList.length; i++) {
                printCpu(i+": ", cpuList[i]);
            }
            Thread.sleep(sleep);
        }
    }
}
