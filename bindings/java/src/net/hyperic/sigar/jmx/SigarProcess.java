package net.hyperic.sigar.jmx;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarProxyCache;

/**
 * Implement the SigarProcessMBean to provide current process info
 * via JMX.
 */

public class SigarProcess implements SigarProcessMBean {

    private Sigar sigar;
    private SigarProxy sigarProxy;

    private static final String procMemName =
        SigarInvokerJMX.getObjectName("ProcMem", "$$");

    private static final String procTimeName =
        SigarInvokerJMX.getObjectName("ProcTime", "$$");

    private SigarInvokerJMX procMem, procTime;

    public SigarProcess() {
        this(null);
    }

    public SigarProcess(String path) {
        sigar = new Sigar();
        sigarProxy = SigarProxyCache.newInstance(sigar);

        procMem = SigarInvokerJMX.getInstance(sigarProxy,
                                              procMemName);

        procTime = SigarInvokerJMX.getInstance(sigarProxy,
                                               procTimeName);
    }

    public void close() {
    }

    private synchronized Long getLongValue(SigarInvokerJMX invoker, String attr) {
        try {
            return (Long)invoker.invoke(attr);
        } catch (SigarException e) {
            return new Long(-1);
        }
    }

    public Long getMemSize() {
        return getLongValue(procMem, "Size");
    }

    /**
     * @deprecated
     * @see getMemSize
     */
    public Long getMemVsize() {
        return getMemSize();
    }

    public Long getMemResident() {
        return getLongValue(procMem, "Resident");
    }

    public Long getMemShare() {
        return getLongValue(procMem, "Share");
    }

    public Long getMemPageFaults() {
        return getLongValue(procMem, "PageFaults");
    }

    public Long getTimeUser() {
        return getLongValue(procTime, "User");
    }

    public Long getTimeSys() {
        return getLongValue(procTime, "Sys");
    }

    public static void main(String args[]) {
        SigarProcessMBean proc = new SigarProcess();
        System.out.println("MemSize=" + proc.getMemSize());
        System.out.println("MemResident=" + proc.getMemResident());
        System.out.println("MemShared=" + proc.getMemShare());
        System.out.println("MemPageFaults=" + proc.getMemPageFaults());
        System.out.println("TimeUser=" + proc.getTimeUser());
        System.out.println("TimeSys=" + proc.getTimeSys());
    }
}
