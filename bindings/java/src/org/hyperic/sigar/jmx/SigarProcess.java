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

package org.hyperic.sigar.jmx;

import org.hyperic.sigar.ProcCpu;
import org.hyperic.sigar.ProcFd;
import org.hyperic.sigar.ProcMem;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarProxyCache;

/**
 * Implement the SigarProcessMBean to provide current process info
 * via JMX.
 */

public class SigarProcess implements SigarProcessMBean {

    private Sigar sigarImpl;
    private SigarProxy sigar;

    public SigarProcess() {
        this(new Sigar());
    }

    public SigarProcess(Sigar sigar) {
        this.sigarImpl = sigar;
        this.sigar = SigarProxyCache.newInstance(sigarImpl);
    }

    public void close() {
        this.sigarImpl.close();
    }

    private RuntimeException unexpectedError(String type,
                                             SigarException e) {
        String msg =
            "Unexected error in Sigar.get" + type +
            ": " + e.getMessage();
        return new IllegalArgumentException(msg);
    }
                                   
    private synchronized ProcMem getMem() {
        try {
            long pid = this.sigar.getPid();
            return this.sigar.getProcMem(pid);
        } catch (SigarException e) {
            throw unexpectedError("Mem", e);
        }
    }

    private synchronized ProcCpu getCpu() {
        try {
            long pid = this.sigar.getPid();
            return this.sigar.getProcCpu(pid);
        } catch (SigarException e) {
            throw unexpectedError("Cpu", e);
        }   
    }

    private synchronized ProcFd getFd() {
        try {
            long pid = this.sigar.getPid();
            return this.sigar.getProcFd(pid);
        } catch (SigarException e) {
            throw unexpectedError("Fd", e);
        }   
    }
    
    public Long getMemSize() {
        return new Long(getMem().getSize());
    }

    /**
     * @deprecated
     * @see getMemSize
     */
    public Long getMemVsize() {
        return getMemSize();
    }

    public Long getMemResident() {
        return new Long(getMem().getResident());
    }

    public Long getMemShare() {
        return new Long(getMem().getShare());
    }

    public Long getMemPageFaults() {
        return new Long(getMem().getPageFaults());
    }

    public Long getTimeUser() {
        return new Long(getCpu().getUser());
    }

    public Long getTimeSys() {
        return new Long(getCpu().getSys());
    }

    public Double getCpuUsage() {
        return new Double(getCpu().getPercent());
    }

    public Long getOpenFd() {
        return new Long(getFd().getTotal());
    }

    public static void main(String args[]) {
        SigarProcessMBean proc = new SigarProcess();
        System.out.println("MemSize=" + proc.getMemSize());
        System.out.println("MemResident=" + proc.getMemResident());
        System.out.println("MemShared=" + proc.getMemShare());
        System.out.println("MemPageFaults=" + proc.getMemPageFaults());
        System.out.println("TimeUser=" + proc.getTimeUser());
        System.out.println("TimeSys=" + proc.getTimeSys());
        System.out.println("OpenFd=" + proc.getOpenFd());
    }
}
