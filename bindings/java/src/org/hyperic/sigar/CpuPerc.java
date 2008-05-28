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

/**
 * CPU percentage usage
 */
public class CpuPerc implements java.io.Serializable {

    private static final long serialVersionUID = 05242007L;

    private double user;
    private double sys;
    private double nice;
    private double idle;
    private double wait;
    private double irq;
    private double softIrq;
    private double stolen;
    private double combined;

    CpuPerc() {}

    native void gather(Sigar sigar, Cpu oldCpu, Cpu curCpu);

    static CpuPerc fetch(Sigar sigar, Cpu oldCpu, Cpu curCpu) {
        CpuPerc perc = new CpuPerc();
        perc.gather(sigar, oldCpu, curCpu);
        return perc;
    }

    /**
     * @deprecated
     */
    public static CpuPerc calculate(Cpu oldCpu, Cpu curCpu) {
        Sigar sigar = new Sigar();
        try {
            return fetch(sigar, oldCpu, curCpu);
        } finally {
            sigar.close();
        }
    }

    public double getUser() {
        return this.user;
    }

    public double getSys() {
        return this.sys;
    }

    public double getNice() {
        return this.nice;
    }

    public double getIdle() {
        return this.idle;
    }

    public double getWait() {
        return this.wait;
    }

    public double getIrq() {
        return this.irq;
    }

    public double getSoftIrq() {
        return this.softIrq;
    }

    public double getStolen() {
        return this.stolen;
    }

    /**
     * @return Sum of User + Sys + Nice + Wait
     */ 
    public double getCombined() {
        return this.combined;
    }

    public static String format(double val) {
        String p = String.valueOf(val * 100.0);
        //cant wait for sprintf.
        int ix = p.indexOf(".") + 1;
        String percent =
            p.substring(0, ix) + 
            p.substring(ix, ix+1);
        return percent + "%";
    }

    public String toString() {
        return
            "CPU states: " +
            format(this.user) + " user, " +
            format(this.sys)  + " system, " +
            format(this.nice) + " nice, " +
            format(this.wait) + " wait, " +
            format(this.idle) + " idle";
    }
}
