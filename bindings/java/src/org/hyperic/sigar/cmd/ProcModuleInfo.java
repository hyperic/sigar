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

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;

/**
 * Display process module info.
 */
public class ProcModuleInfo extends SigarCommandBase {

    public ProcModuleInfo(Shell shell) {
        super(shell);
    }

    public ProcModuleInfo() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display process module info";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        long[] pids = this.shell.findPids(args);

        for (int i=0; i<pids.length; i++) {
            try {
                output(pids[i]);
            } catch (SigarException e) {
                println("(" + e.getMessage() + ")");
            }
            println("\n------------------------\n");
        }
    }

    public void output(long pid) throws SigarException {
        println("pid=" + pid);

        try {
            List modules = this.sigar.getProcModules(pid);

            for (int i=0; i<modules.size(); i++) {
                println(i + "=" + modules.get(i));
            }
        } catch (SigarNotImplementedException e) {
            throw e;
        } catch (SigarException e) {
            println("[" + e.getMessage() + "]");
        }
    }

    public static void main(String[] args) throws Exception {
        new ProcModuleInfo().processCommand(args);
    }
}
