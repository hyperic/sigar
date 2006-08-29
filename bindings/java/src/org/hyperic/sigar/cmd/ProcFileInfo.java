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
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.SigarPermissionDeniedException;
import org.hyperic.sigar.ProcFd;
import org.hyperic.sigar.ProcExe;

/**
 * Display process file information.
 */
public class ProcFileInfo extends SigarCommandBase {

    public ProcFileInfo(Shell shell) {
        super(shell);
    }

    public ProcFileInfo() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display process file info";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        long[] pids = this.shell.findPids(args);

        for (int i=0; i<pids.length; i++) {
            try {
                output(pids[i]);
            } catch (SigarPermissionDeniedException e) {
                println(this.shell.getUserDeniedMessage(pids[i]));
            } catch (SigarException e) {
                println("(" + e.getMessage() + ")");
            }
            println("\n------------------------\n");
        }
    }

    public void output(long pid) throws SigarException {
        println("pid=" + pid);

        try {
            ProcFd fd = sigar.getProcFd(pid);
            println("open file descriptors=" + fd.getTotal());
        } catch (SigarNotImplementedException e) {}

        ProcExe exe = sigar.getProcExe(pid);
        String name = exe.getName();
        if (name.length() == 0) {
            name = "unknown";
        }
        println("name=" + name);

        println("cwd=" + exe.getCwd());
    }

    public static void main(String[] args) throws Exception {
        new ProcFileInfo().processCommand(args);
    }
}
