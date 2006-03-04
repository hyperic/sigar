package net.hyperic.sigar.jmx;

import net.hyperic.sigar.ProcFd;
import net.hyperic.sigar.ProcMem;
import net.hyperic.sigar.ProcTime;
import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarProxyCache;

/**
 * Implement the SigarProcessMBean to provide current process info
 * via JMX.
 */

public class SigarProcess implements SigarProcessMBean {

    private Sigar sigarImpl;
    private SigarProxy sigar;

    public SigarProcess() {
        this.sigarImpl = new Sigar();
        this.sigar = SigarProxyCache.newInstance(sigarImpl);
    }

    public void close() {
        this.sigarImpl.close();
    }

    private ProcMem getMem() {
        try {
            long pid = this.sigar.getPid();
            return this.sigar.getProcMem(pid);
        } catch (SigarException e) {
            throw new IllegalArgumentException();
        }
    }

    private ProcTime getTime() {
        try {
            long pid = this.sigar.getPid();
            return this.sigar.getProcTime(pid);
        } catch (SigarException e) {
            throw new IllegalArgumentException();
        }   
    }

    private ProcFd getFd() {
        try {
            long pid = this.sigar.getPid();
            return this.sigar.getProcFd(pid);
        } catch (SigarException e) {
            throw new IllegalArgumentException();
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
        return new Long(getTime().getUser());
    }

    public Long getTimeSys() {
        return new Long(getTime().getSys());
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
