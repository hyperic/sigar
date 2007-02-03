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

import org.hyperic.sigar.jmx.CpuTimerMBean;

import java.io.PrintStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class CpuTimer implements CpuTimerMBean {
    private static final Map timers =
        Collections.synchronizedMap(new HashMap());

    private Sigar sigar;
    private long totalTime;
    private long cpuTotal;
    private long cpuUser;
    private long cpuSys;
    private long cpuSampleFirst;
    private long cpuSampleLast;
    private long cpuSampleTime;

    private ThreadCpu cpu = new ThreadCpu();

    private long startTime, stopTime;

    public CpuTimer() {
        this(null);
    }

    public CpuTimer(Sigar sigar) {
        clear();
        this.sigar = sigar;
    }

    public void clear() {
        this.startTime = -1;
        this.stopTime = -1;
        this.totalTime = 0;
        this.cpuTotal = 0;
        this.cpuUser  = 0;
        this.cpuSys   = 0;
        this.cpuSampleFirst = 0;
        this.cpuSampleLast = 0;
        this.cpuSampleTime = 0;
    }

    private void stamp(CpuTimer timer) {
        if (this.cpuSampleFirst == 0) {
            this.cpuSampleFirst = toMillis(timer.cpu.total);
            this.cpuSampleTime = timer.startTime;
        }
        else {
            this.cpuSampleLast = toMillis(timer.cpu.total);
        }
    }

    public void add(CpuTimer timer) {
        stamp(timer);
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
        stamp(this);
    }

    public void stop() {
        stop(this.sigar);
    }

    public void stop(Sigar sigar) {
        ThreadCpu diff = getDiff(sigar);

        this.cpuTotal += diff.total;
        this.cpuUser  += diff.user;
        this.cpuSys   += diff.sys;

        this.stopTime = System.currentTimeMillis();
        
        double timeDiff = this.stopTime - this.startTime;

        this.totalTime += timeDiff;
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

        stamp(this);

        return diff;
    }

    public long getTotalTime() {
        return this.totalTime;
    }
    
    private long toMillis(long ns) {
        return ns / 1000000; //convert nanos to millis
    }

    public long getCpuTotal() {
        return toMillis(this.cpuTotal);
    }
    
    public long getCpuUser() {
        return toMillis(this.cpuUser);
    }
    
    public long getCpuSys() {
        return toMillis(this.cpuSys);
    }

    //expecting start/stop/add to be called more often than this,
    //so we give percentage over time in between calls.
    public double getCpuUsage() {
        if ((this.cpuSampleFirst == 0) ||
            (this.cpuSampleLast == 0))
        {
            return 0.0;
        }

        long timeNow = System.currentTimeMillis();
        double diff = timeNow - this.cpuSampleTime;
        if (diff == 0) {
            return 0.0;
        }

        double usage =
            (this.cpuSampleLast - this.cpuSampleFirst) / diff;

        this.cpuSampleFirst = 0;
        this.cpuSampleLast = 0;
        this.cpuSampleTime = 0;

        return usage;
    }

    public long getLastSampleTime() {
        return this.stopTime;
    }

    public static CpuTimer getInstance(String name) {
        CpuTimer timer = (CpuTimer)timers.get(name);
        if (timer == null) {
            timer = new CpuTimer();
            timers.put(name, timer);
        }
        return timer;
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
        out.println("usage...." + CpuPerc.format(getCpuUsage()));
    }
}
