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

public class Pidof extends SigarCommandBase {

    public Pidof(Shell shell) {
        super(shell);
    }

    public Pidof() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length > 0;
    }

    public String getSyntaxArgs() {
        return "query";
    }

    public String getUsageShort() {
        return "Find the process ID of a running program";
    }

    public void output(String[] args) throws SigarException {
        long[] pids = this.shell.findPids(args);

        for (int i=0; i<pids.length; i++) {
            this.out.print(pids[i]);
            this.out.print(' ');
        }
        this.out.println();
    }
}
