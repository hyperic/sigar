package net.hyperic.sigar;

import java.util.List;
import java.util.Map;

/**
 * Safely share a single Sigar instance between multiple threads.
 * The Sigar object is thread-safe given that each thread has its
 * own Sigar instance.  In order to share a single Sigar instance
 * the methods implemented here are synchronized.  All data returned
 * by the Sigar methods is read-only and hence thread-safe.
 */
public class SynchronizedSigar implements SigarProxy {

    private Sigar sigar;

    public SynchronizedSigar() {
        this.sigar = new Sigar();
    }

    public synchronized long getPid()
    {
        return this.sigar.getPid();
    }

    public synchronized Mem getMem()
        throws SigarException
    {
        return this.sigar.getMem();
    }

    public synchronized Swap getSwap()
        throws SigarException
    {
        return this.sigar.getSwap();
    }

    public synchronized Cpu getCpu()
        throws SigarException
    {
        return this.sigar.getCpu();
    }

    public synchronized CpuPerc getCpuPerc()
        throws SigarException
    {
        return this.sigar.getCpuPerc();
    }

    public synchronized Uptime getUptime()
        throws SigarException
    {
        return this.sigar.getUptime();
    }

    public synchronized double[] getLoadAverage()
        throws SigarException
    {
        return this.sigar.getLoadAverage();
    }

    public synchronized long[] getProcList()
        throws SigarException
    {
        return this.sigar.getProcList();
    }

    public synchronized ProcStat getProcStat()
        throws SigarException
    {
        return this.sigar.getProcStat();
    }

    public synchronized ProcMem getProcMem(long pid)
        throws SigarException
    {
        return this.sigar.getProcMem(pid);
    }

    public synchronized ProcMem getProcMem(String pid)
        throws SigarException
    {
        return this.sigar.getProcMem(pid);
    }

    public synchronized ProcState getProcState(long pid)
        throws SigarException
    {
        return this.sigar.getProcState(pid);
    }

    public synchronized ProcState getProcState(String pid)
        throws SigarException
    {
        return this.sigar.getProcState(pid);
    }

    public synchronized ProcTime getProcTime(long pid)
        throws SigarException
    {
        return this.sigar.getProcTime(pid);
    }

    public synchronized ProcTime getProcTime(String pid)
        throws SigarException
    {
        return this.sigar.getProcTime(pid);
    }

    public synchronized ProcCpu getProcCpu(long pid)
        throws SigarException
    {
        return this.sigar.getProcCpu(pid);
    }

    public synchronized ProcCpu getProcCpu(String pid)
        throws SigarException
    {
        return this.sigar.getProcCpu(pid);
    }

    public synchronized ProcCred getProcCred(long pid)
        throws SigarException
    {
        return this.sigar.getProcCred(pid);
    }

    public synchronized ProcCred getProcCred(String pid)
        throws SigarException
    {
        return this.sigar.getProcCred(pid);
    }

    public synchronized ProcCredName getProcCredName(long pid)
        throws SigarException
    {
        return this.sigar.getProcCredName(pid);
    }

    public synchronized ProcCredName getProcCredName(String pid)
        throws SigarException
    {
        return this.sigar.getProcCredName(pid);
    }

    public synchronized ProcFd getProcFd(long pid)
        throws SigarException
    {
        return this.sigar.getProcFd(pid);
    }

    public synchronized ProcFd getProcFd(String pid)
        throws SigarException
    {
        return this.sigar.getProcFd(pid);
    }

    public synchronized ProcExe getProcExe(long pid)
        throws SigarException
    {
        return this.sigar.getProcExe(pid);
    }

    public synchronized ProcExe getProcExe(String pid)
        throws SigarException
    {
        return this.sigar.getProcExe(pid);
    }

    public synchronized String[] getProcArgs(long pid)
        throws SigarException
    {
        return this.sigar.getProcArgs(pid);
    }

    public synchronized String[] getProcArgs(String pid)
        throws SigarException
    {
        return this.sigar.getProcArgs(pid);
    }

    public synchronized Map getProcEnv(long pid)
        throws SigarException
    {
        return this.sigar.getProcEnv(pid);
    }

    public synchronized Map getProcEnv(String pid)
        throws SigarException
    {
        return this.sigar.getProcEnv(pid);
    }

    public synchronized String getProcEnv(long pid, String key)
        throws SigarException
    {
        return this.sigar.getProcEnv(pid, key);
    }

    public synchronized String getProcEnv(String pid, String key)
        throws SigarException
    {
        return this.sigar.getProcEnv(pid, key);
    }

    public synchronized List getProcModules(long pid)
        throws SigarException
    {
        return this.sigar.getProcModules(pid);
    }

    public synchronized List getProcModules(String pid)
        throws SigarException
    {
        return this.sigar.getProcModules(pid);
    }

    public synchronized long getProcPort(long port)
        throws SigarException
    {
        return this.sigar.getProcPort(port);
    }

    public synchronized long getProcPort(String port)
        throws SigarException
    {
        return this.sigar.getProcPort(port);
    }

    public synchronized FileSystem[] getFileSystemList()
        throws SigarException
    {
        return this.sigar.getFileSystemList();
    }

    public synchronized FileSystemMap getFileSystemMap()
        throws SigarException
    {
        return this.sigar.getFileSystemMap();
    }

    public synchronized FileSystemUsage getMountedFileSystemUsage(String name)
        throws SigarException
    {
        return this.sigar.getMountedFileSystemUsage(name);
    }

    public synchronized FileSystemUsage getFileSystemUsage(String name)
        throws SigarException
    {
        return this.sigar.getFileSystemUsage(name);
    }

    public synchronized FileInfo getFileInfo(String name)
        throws SigarException
    {
        return this.sigar.getFileInfo(name);
    }

    public synchronized FileInfo getLinkInfo(String name)
        throws SigarException
    {
        return this.sigar.getLinkInfo(name);
    }

    public synchronized DirStat getDirStat(String name)
        throws SigarException
    {
        return this.sigar.getDirStat(name);
    }

    public synchronized CpuInfo[] getCpuInfoList()
        throws SigarException
    {
        return this.sigar.getCpuInfoList();
    }

    public synchronized Cpu[] getCpuList()
        throws SigarException
    {
        return this.sigar.getCpuList();
    }

    public synchronized CpuPerc[] getCpuPercList()
        throws SigarException
    {
        return this.sigar.getCpuPercList();
    }

    public synchronized NetRoute[] getNetRouteList()
        throws SigarException
    {
        return this.sigar.getNetRouteList();
    }

    public synchronized NetInterfaceConfig getNetInterfaceConfig(String name)
        throws SigarException
    {
        return this.sigar.getNetInterfaceConfig(name);
    }

    public synchronized NetInterfaceStat getNetInterfaceStat(String name)
        throws SigarException
    {
        return this.sigar.getNetInterfaceStat(name);
    }

    public synchronized String[] getNetInterfaceList()
        throws SigarException
    {
        return this.sigar.getNetInterfaceList();
    }

    public synchronized String getFQDN()
        throws SigarException
    {
        return this.sigar.getFQDN();
    }
}
