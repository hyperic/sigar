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

import java.util.Map;
import java.util.Iterator;

import org.hyperic.sigar.SigarException;

/**
 * Show process environment.
 */
public class ShowEnv extends SigarCommandBase {

    public ShowEnv(Shell shell) {
        super(shell);
    }

    public ShowEnv() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Show process environment";
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
        Map env = this.proxy.getProcEnv(pid);

        for (Iterator it = env.entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry ent = (Map.Entry)it.next();

            println(ent.getKey() + "=" + ent.getValue());
        }
    }

    public static void main(String[] args) throws Exception {
        new ShowEnv().processCommand(args);
    }
}
