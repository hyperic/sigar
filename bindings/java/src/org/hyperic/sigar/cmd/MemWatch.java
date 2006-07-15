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
import org.hyperic.sigar.ProcMem;

/**
 * Watch for changes in program memory usage.
 */
public class MemWatch {

    static final int SLEEP_TIME = 1000 * 10;

    public static void main(String[] args) throws Exception {
        Sigar sigar = new Sigar();

        if (args.length != 1) {
            throw new Exception("Usage: MemWatch pid");
        }

        long pid = Long.parseLong(args[0]);

        long lastTime = System.currentTimeMillis();

        ProcMem last = sigar.getProcMem(pid);

        while (true) {
            ProcMem cur = sigar.getProcMem(pid);
            
            StringBuffer diff = diff(last, cur);

            if (diff.length() == 0) {
                System.out.println("no change " +
                                   "(size=" +
                                   Sigar.formatSize(cur.getSize()) +
                                   ")");
            }
            else {
                long curTime = System.currentTimeMillis();
                long timeDiff = curTime - lastTime;
                lastTime = curTime;
                diff.append(" after " + timeDiff + "ms");
                System.out.println(diff);
            }

            last = cur;
            Thread.sleep(SLEEP_TIME);
        }
    }

    private static StringBuffer diff(ProcMem last, ProcMem cur) {
        StringBuffer buf = new StringBuffer();

        long diff;

        diff = cur.getSize() - last.getSize();
        if (diff != 0) {
            buf.append("size=" + diff);
        }

        diff = cur.getResident() - last.getResident();
        if (diff != 0) {
            buf.append(", resident=" + diff);
        }

        diff = cur.getShare() - last.getShare();
        if (diff != 0) {
            buf.append(", share=" + diff);
        }

        return buf;
    }
}

            
