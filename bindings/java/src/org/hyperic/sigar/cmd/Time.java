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
import org.hyperic.sigar.CpuTimer;

public class Time extends SigarCommandBase {

    public Time(Shell shell) {
        super(shell);
    }

    public Time() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length >= 1;
    }

    public String getSyntaxArgs() {
        return "[command] [...]";
    }

    public String getUsageShort() {
        return "Time command";
    }

    public void output(String[] args) throws SigarException {
        boolean isInteractive = this.shell.isInteractive();
        //turn off paging.
        this.shell.setInteractive(false);
        CpuTimer cpu = new CpuTimer(this.sigar);

        int num;
        
        if (Character.isDigit(args[0].charAt(0))) {
            num = Integer.parseInt(args[0]);
            String[] xargs = new String[args.length-1];
            System.arraycopy(args, 1, xargs, 0, xargs.length);
            args = xargs;
        }
        else {
            num = 1;
        }

        cpu.start();

        try {
            for (int i=0; i<num; i++) {
                this.shell.handleCommand("time " + args[0], args);
            }
        } finally {
            this.shell.setInteractive(isInteractive);
        }

        cpu.stop();
        cpu.list(this.out);
    }

    public static void main(String[] args) throws Exception {
        new Time().processCommand(args);
    }
}
