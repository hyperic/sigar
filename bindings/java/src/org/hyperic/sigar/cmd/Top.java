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

package org.hyperic.sigar.cmd;

import java.util.List;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.CpuPerc;
import org.hyperic.sigar.SigarProxyCache;
import org.hyperic.sigar.ProcCpu;
import org.hyperic.sigar.ProcStat;

/**
 * Display system resource utilization summaries and process information.
 * <p>
 * This version of the top command requires a ptql query to select which
 * processes to display.
 *
 * Example to display java processes only:<br>
 * <code>% java -jar sigar-bin/lib/sigar.jar Top State.Name.eq=java</code>
 */

public class Top {
    private static final int SLEEP_TIME = 1000 * 5;

    private static final String HEADER =
        "PID\tUSER\tSTIME\tSIZE\tRSS\tSHARE\tSTATE\tTIME\t%CPU\tCOMMAND";

    private static String toString(ProcStat stat) {
        return 
            stat.getTotal()    + " processes: " +
            stat.getSleeping() + " sleeping, " +
            stat.getRunning()  + " running, " + 
            stat.getZombie()   + " zombie, " +
            stat.getStopped()  + " stopped... " + stat.getThreads() + " threads";
    }

    public static void main(String[] args) throws Exception {
        Sigar sigarImpl = new Sigar();

        SigarProxy sigar = 
            SigarProxyCache.newInstance(sigarImpl, SLEEP_TIME);

        while (true) {
            Shell.clearScreen();

            System.out.println(Uptime.getInfo(sigar));

            System.out.println(toString(sigar.getProcStat()));

            System.out.println(sigar.getCpuPerc());

            System.out.println(sigar.getMem());

            System.out.println(sigar.getSwap());
                               
            System.out.println();

            System.out.println(HEADER);

            long[] pids = Shell.getPids(sigar, args);

            for (int i=0; i<pids.length; i++) {
                long pid = pids[i];

                String cpuPerc = "?";

                List info;
                try {
                    info = Ps.getInfo(sigar, pid);
                } catch (SigarException e) {
                    continue; //process may have gone away
                }
                try {
                    ProcCpu cpu = sigar.getProcCpu(pid);
                    cpuPerc = CpuPerc.format(cpu.getPercent());
                } catch (SigarException e) {
                }

                info.add(info.size()-1, cpuPerc);

                System.out.println(Ps.join(info));
            }

            Thread.sleep(SLEEP_TIME);
            SigarProxyCache.clear(sigar);
        }
    }
}
