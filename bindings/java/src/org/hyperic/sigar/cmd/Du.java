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

import org.hyperic.sigar.DirUsage;
import org.hyperic.sigar.SigarException;

/**
 * Display usage for a directory recursively
 */
public class Du extends SigarCommandBase {

    //like du -s -b

    public Du(Shell shell) {
        super(shell);
    }

    public Du() {
        super();
    }

    public String getUsageShort() {
        return "Display usage for a directory recursively";
    }

    protected boolean validateArgs(String[] args) {
        return args.length == 1;
    }

    public void output(String[] args) throws SigarException {
        String dir = args[0];
        DirUsage du = this.sigar.getDirUsage(dir);
        println(du.getDiskUsage() + "\t" + dir);
    }

    public static void main(String[] args) throws Exception {
        new Du().processCommand(args);
    }
}
