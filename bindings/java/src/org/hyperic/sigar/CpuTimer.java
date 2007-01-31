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

package org.hyperic.sigar;

import java.io.PrintStream;

public class CpuTimer {
    private Sigar sigar;
    private long totalTime;
    private long cpuTotal;
    private long cpuUser;
    private long cpuSys;
    
    private ThreadCpu cpu = new ThreadCpu();

    private long startTime;

    public CpuTimer() {
        this(null);
    }

    public CpuTimer(Sigar sigar) {
        clear();
        this.sigar = sigar;
    }

    public void clear() {
        this.totalTime = 0;
        this.cpuTotal = 0;
        this.cpuUser  = 0;
        this.cpuSys   = 0;
    }

    public void add(CpuTimer timer) {
        this.cpuTotal  += timer.cpuTotal;
        this.cpuUser   += timer.cpuUser;
        this.cpuSys    += timer.cpuSys;
        this.totalTime += timer.totalTime;
    }

    public void start() {
        start(this.sigar);
    }

    public void start(Sigar sigar) {
        this.startTime = System.currentTimeMillis();

        try {
            this.cpu.gather(sigar, 0);
        } catch (SigarException e) {
            throw new IllegalArgumentException(e.toString());
        }
    }

    public void stop() {
        stop(this.sigar);
    }

    public void stop(Sigar sigar) {
        ThreadCpu diff = getDiff(sigar);

        this.cpuTotal += diff.total;
        this.cpuUser  += diff.user;
        this.cpuSys   += diff.sys;

        long stopTime = System.currentTimeMillis();
        
        long time = stopTime - this.startTime;

        this.totalTime += time;
    }

    public ThreadCpu getDiff() {
        return getDiff(this.sigar);
    }

    public ThreadCpu getDiff(Sigar sigar) {
        long startTotal = this.cpu.total;
        long startUser  = this.cpu.user;
        long startSys   = this.cpu.sys;
    
        ThreadCpu diff = new ThreadCpu();

        try {
            this.cpu.gather(sigar, 0);
        } catch (SigarException e) {
            throw new IllegalArgumentException(e.toString());
        }

        diff.total = this.cpu.total - startTotal;
        diff.user  = this.cpu.user - startUser;
        diff.sys   = this.cpu.sys - startSys;

        return diff;
    }

    public long getTotalTime() {
        return this.totalTime;
    }
    
    public long getCpuTotal() {
        return this.cpuTotal / 1000000; //convert nanos to millis
    }
    
    public long getCpuUser() {
        return this.cpuUser / 1000000; //convert nanos to millis
    }
    
    public long getCpuSys() {
        return this.cpuSys / 1000000; //convert nanos to millis
    }

    public String format(long elap) {
        String fraction = (elap % 1000) + "";
        int pad = 3 - fraction.length();

        StringBuffer buf = new StringBuffer()
            .append(elap / 1000).append('.');

        //for example, 15 millseconds formatted as ".015" rather than ".15"
        while (pad-- > 0) {
            buf.append("0");
        }
        buf.append(fraction).append(" seconds");
        return buf.toString();
    }

    public void list(PrintStream out) {
        out.println("real....." +
                    format(getTotalTime()));
        out.println("user....." + format(getCpuUser()));
        out.println("sys......" + format(getCpuSys()));
    }
}
