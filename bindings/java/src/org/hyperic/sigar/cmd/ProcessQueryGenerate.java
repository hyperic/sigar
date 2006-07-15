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

import org.hyperic.sigar.ptql.ProcessQueryGenerator;

public class ProcessQueryGenerate extends SigarCommandBase {

    public ProcessQueryGenerate(Shell shell) {
        super(shell);
    }

    public ProcessQueryGenerate() {
        super();
    }

    public boolean validateArgs(String[] args) {
        return true;
    }

    public void output(String[] args) throws SigarException {
        ProcessQueryGenerator generator =
            new ProcessQueryGenerator(this.proxy);

        long[] pids;

        if (args.length > 0) {
            pids = this.shell.findPids(args);
        }
        else {
            pids = this.proxy.getProcList();
        }

        for (int i=0; i<pids.length; i++) {
            long pid = pids[i];
            String query = generator.generate(pid);

            if (query != null) {
                println(query);
            }
            else {
                this.err.println("failed to narrow query for " + pid +
                                 " (" +
                                 this.proxy.getProcState(pid).getName() +
                                 ")");
            }
        }

        flush();
    }

    public static void main(String[] args) throws Exception {
        new ProcessQueryGenerate().processCommand(args);
    }
}
