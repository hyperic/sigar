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

import java.util.Arrays;

import org.hyperic.sigar.SigarException;

/**
 * Display System Information
 */
public class SysInfo extends SigarCommandBase {

    public SysInfo(Shell shell) {
        super(shell);
    }

    public SysInfo() {
        super();
    }

    public String getUsageShort() {
        return "Display system information";
    }

    public void output(String[] args) throws SigarException {
        //sigar/os info
        Version.printInfo(this.out);
        println("");

        //uptime
        new Uptime(this.shell).output(args);
        println("");

        //cpu info
        CpuInfo cpuinfo = new CpuInfo(this.shell);
        cpuinfo.displayTimes = false;
        cpuinfo.output(args);
        println("");

        //memory info
        new Free(this.shell).output(args);
        println("");

        println("File Systems........." +
                Arrays.asList(this.sigar.getFileSystemList()));
        println("");

        println("Network Interfaces..." +
                Arrays.asList(this.sigar.getNetInterfaceList()));
        println("");

        //system resource limits
        println("System resource limits:");
        new Ulimit(this.shell).output(args);
    }

    public static void main(String[] args) throws Exception {
        new SysInfo().processCommand(args);
    }
}
