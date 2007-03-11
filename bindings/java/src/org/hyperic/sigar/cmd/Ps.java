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

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.ProcCredName;
import org.hyperic.sigar.ProcMem;
import org.hyperic.sigar.ProcTime;
import org.hyperic.sigar.ProcState;
import org.hyperic.sigar.ProcUtil;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Show process status.
 */
public class Ps extends SigarCommandBase {

    public Ps(Shell shell) {
        super(shell);
    }

    public Ps() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getSyntaxArgs() {
        return "[pid|query]";
    }

    public String getUsageShort() {
        return "Show process status";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        long[] pids;
        if (args.length == 0) {
            pids = this.proxy.getProcList();
        }
        else {
            pids = this.shell.findPids(args);
        }

        for (int i=0; i<pids.length; i++) {
            long pid = pids[i];
            try {
                output(pid);
            } catch (SigarException e) {
                this.err.println("Exception getting process info for " +
                                 pid + ": " + e.getMessage());
            }
        }
    }

    public static String join(List info) {
        StringBuffer buf = new StringBuffer();
        Iterator i = info.iterator();
        boolean hasNext = i.hasNext();
        while (hasNext) {
            buf.append((String)i.next());
            hasNext = i.hasNext();
            if (hasNext)
                buf.append("\t");
        }

        return buf.toString();
    }

    public static List getInfo(SigarProxy sigar, long pid)
        throws SigarException {

        ProcState state = sigar.getProcState(pid);
        ProcTime time = null;
        String unknown = "???";

        List info = new ArrayList();
        info.add(String.valueOf(pid));

        try {
            ProcCredName cred = sigar.getProcCredName(pid);
            info.add(cred.getUser());
        } catch (SigarException e) {
            info.add(unknown);
        }

        try {
            time = sigar.getProcTime(pid);
            info.add(getStartTime(time.getStartTime()));
        } catch (SigarException e) {
            info.add(unknown);
        }

        try {
            ProcMem mem = sigar.getProcMem(pid);
            info.add(Sigar.formatSize(mem.getSize()));
            info.add(Sigar.formatSize(mem.getRss()));
            info.add(Sigar.formatSize(mem.getShare()));
        } catch (SigarException e) {
            info.add(unknown);
        }

        info.add(String.valueOf(state.getState()));

        if (time != null) {
            info.add(getCpuTime(time));
        }
        else {
            info.add(unknown);
        }

        String name = ProcUtil.getDescription(sigar, pid);
        info.add(name);

        return info;
    }

    public void output(long pid) throws SigarException {
        println(join(getInfo(this.proxy, pid)));
    }

    public static String getCpuTime(long total) {
        long t = total / 1000;
        return t/60 + ":" + t%60;
    }

    public static String getCpuTime(ProcTime time) {
        return getCpuTime(time.getTotal());
    }

    private static String getStartTime(long time) {
        if (time == 0) {
            return "00:00";
        }
        long timeNow = System.currentTimeMillis();
        String fmt = "MMMd";

        if ((timeNow - time) < ((60*60*24) * 1000)) {
            fmt = "HH:mm";
        }

        return new SimpleDateFormat(fmt).format(new Date(time));
    }

    public static void main(String[] args) throws Exception {
        new Ps().processCommand(args);
    }
}

            
