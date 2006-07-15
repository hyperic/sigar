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

/**
 * Show process command line arguments.
 */
public class ShowArgs extends SigarCommandBase {

    public ShowArgs(Shell shell) {
        super(shell);
    }

    public ShowArgs() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Show process command line arguments";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        long[] pids = this.shell.findPids(args);

        for (int i=0; i<pids.length; i++) {
            try {
                println("pid=" + pids[i]);
                output(pids[i]);
            } catch (SigarException e) {
                println(e.getMessage());
            }
            println("\n------------------------\n");
        }
    }

    public void output(long pid) throws SigarException {

        String[] argv = this.proxy.getProcArgs(pid);
        
        try {
            String exe = this.proxy.getProcExe(pid).getName();
            println("exe=" + exe);
        } catch (SigarNotImplementedException e) {
        } catch (SigarException e) {
            println("exe=???");
        }

        try {
            String cwd = this.proxy.getProcExe(pid).getCwd();
            println("cwd=" + cwd);
        } catch (SigarNotImplementedException e) {
        } catch (SigarException e) {
            println("cwd=???");
        }

        for (int i=0; i<argv.length; i++) {
            println("   " + i + "=>" + argv[i] + "<=");
        }
    }

    public static void main(String[] args) throws Exception {
        new ShowArgs().processCommand(args);
    }
}
