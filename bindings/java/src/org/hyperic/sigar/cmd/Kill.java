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

/**
 * Send a signal to a process.
 */
public class Kill extends SigarCommandBase {

    public Kill(Shell shell) {
        super(shell);
    }

    public Kill() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length == 1 || args.length == 2;
    }

    public String getSyntaxArgs() {
        return "[signal] <query|pid>";
    }

    public String getUsageShort() {
        return "Send signal to a process";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        int signum = 15; //SIGTERM
        long[] pids;
        String query;

        if (args.length == 2) {
            query = args[1];
            try {
                signum = Integer.parseInt(args[0]);
            } catch (NumberFormatException e) {
                //XXX convert SIGFOO to number
                throw new SigarException(e.getMessage());
            }
        }
        else {
            query = args[0];
        }

        pids = this.shell.findPids(new String[] { query });

        for (int i=0; i<pids.length; i++) {
            println("kill " + signum + " " + pids[i]);
            this.sigar.kill(pids[i], signum);
        }
    }

    public static void main(String[] args) throws Exception {
        new Kill().processCommand(args);
    }
}
