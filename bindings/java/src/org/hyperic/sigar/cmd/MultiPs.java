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

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.CpuPerc;
import org.hyperic.sigar.MultiProcCpu;
import org.hyperic.sigar.ProcMem;

/**
 * Show multi process status.
 */
public class MultiPs extends SigarCommandBase {

    public MultiPs(Shell shell) {
        super(shell);
    }

    public MultiPs() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length == 1;
    }

    public String getSyntaxArgs() {
        return "query";
    }

    public String getUsageShort() {
        return "Show multi process status";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        String query = args[0];
        MultiProcCpu cpu = this.proxy.getMultiProcCpu(query);
        println("Number of processes: " + cpu.getProcesses());
        println("Cpu usage: " + CpuPerc.format(cpu.getPercent()));
        println("Cpu time: "  + Ps.getCpuTime(cpu.getTotal()));

        ProcMem mem = this.proxy.getMultiProcMem(query);
        println("Size: " + Sigar.formatSize(mem.getSize()));
        println("Resident: " + Sigar.formatSize(mem.getResident()));
        println("Share: " + Sigar.formatSize(mem.getShare()));
    }

    public static void main(String[] args) throws Exception {
        new MultiPs().processCommand(args);
    }
}

            
