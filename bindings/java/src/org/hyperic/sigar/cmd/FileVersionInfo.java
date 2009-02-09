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

import java.io.File;
import java.util.Iterator;
import java.util.Map;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.ProcExe;
import org.hyperic.sigar.win32.Win32;
import org.hyperic.sigar.win32.FileVersion;

/**
 * Display process file information.
 */
public class FileVersionInfo extends SigarCommandBase {

    public FileVersionInfo(Shell shell) {
        super(shell);
    }

    public FileVersionInfo() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length >= 1;
    }

    public String getUsageShort() {
        return "Display file version info";
    }

    public void output(String[] args) throws SigarException {
        for (int i=0; i<args.length; i++) {
            String exe = args[i];
            if (new File(exe).exists()) {
                output(exe);
            }
            else {
                long[] pids = this.shell.findPids(exe);
                for (int j=0; j<pids.length; j++) {
                    try {
                        output(sigar.getProcExe(pids[j]).getName());
                    } catch (SigarException e) {
                        println(exe + ": " + e.getMessage());
                    }
                }
            }
        }
    }

    private void output(String key, String val) {
        final int max = 20;
        int len = max - key.length();
        StringBuffer sb = new StringBuffer();
        sb.append("  ").append(key);
        while (len-- > 0) {
            sb.append('.');
        }
        sb.append(val);
        println(sb.toString());
    }

    public void output(String exe) throws SigarException {
        FileVersion info = Win32.getFileVersion(exe);
        if (info == null) {
            return;
        }
        println("Version info for file '" + exe + "':");
        output("FileVersion", info.getFileVersion());
        output("ProductVersion", info.getProductVersion());
        for (Iterator it = info.getInfo().entrySet().iterator();
             it.hasNext();)
        {
            Map.Entry entry = (Map.Entry)it.next();
            output((String)entry.getKey(), (String)entry.getValue());
        }
    }

    public static void main(String[] args) throws Exception {
        new FileVersionInfo().processCommand(args);
    }
}
