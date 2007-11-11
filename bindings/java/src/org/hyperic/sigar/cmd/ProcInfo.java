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

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarPermissionDeniedException;

/**
 * Display all process information.
 */
public class ProcInfo extends SigarCommandBase {

    private boolean isSingleProcess;

    public ProcInfo(Shell shell) {
        super(shell);
    }

    public ProcInfo() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display all process info";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        this.isSingleProcess = false;

        if ((args.length != 0) && args[0].startsWith("-s")) {
            this.isSingleProcess = true;
        }

        if (this.isSingleProcess) {
            for (int i=1; i<args.length; i++) {
                try {
                    output(args[i]);
                } catch (SigarException e) {
                    println("(" + e.getMessage() + ")");
                }
                println("\n------------------------\n");
            }
        }
        else {
            long[] pids = this.shell.findPids(args);

            for (int i=0; i<pids.length; i++) {
                try {
                    output(String.valueOf(pids[i]));
                } catch (SigarPermissionDeniedException e) {
                    println(this.shell.getUserDeniedMessage(pids[i]));
                } catch (SigarException e) {
                    println("(" + e.getMessage() + ")");
                }
                println("\n------------------------\n");
            }
        }
    }

    public void output(String pid) throws SigarException {
        println("pid=" + pid);
        try {
            println("state=" + sigar.getProcState(pid));
        } catch (SigarException e) {
            if (this.isSingleProcess) {
                println(e.getMessage());
            }
        }
        try {
            println("mem=" + sigar.getProcMem(pid));
        } catch (SigarException e) {}
        try {
            println("cpu=" + sigar.getProcCpu(pid));
        } catch (SigarException e) {}
        try {
            println("cred=" + sigar.getProcCred(pid));
        } catch (SigarException e) {}
        try {
            println("credname=" + sigar.getProcCredName(pid));
        } catch (SigarException e) {}
    }

    public static void main(String[] args) throws Exception {
        new ProcInfo().processCommand(args);
    }
}
