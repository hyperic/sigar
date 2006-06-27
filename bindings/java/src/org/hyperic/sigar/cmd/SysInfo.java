package org.hyperic.sigar.cmd;

import java.util.Arrays;

import org.hyperic.sigar.SigarException;

/**
 * Display System Information
 */
public class SysInfo extends SigarCommandBase {

    public SysInfo(Shell shell) {
        super(shell);
    }

    public SysInfo() {
        super();
    }

    public String getUsageShort() {
        return "Display system information";
    }

    public void output(String[] args) throws SigarException {
        //sigar/os info
        Version.printInfo(this.out);
        println("");

        //uptime
        new Uptime(this.shell).output(args);
        println("");

        //cpu info
        CpuInfo cpuinfo = new CpuInfo(this.shell);
        cpuinfo.displayTimes = false;
        cpuinfo.output(args);
        println("");

        //memory info
        new Free(this.shell).output(args);
        println("");

        println("File Systems........." +
                Arrays.asList(this.sigar.getFileSystemList()));
        println("");

        println("Network Interfaces..." +
                Arrays.asList(this.sigar.getNetInterfaceList()));
        println("");

        //system resource limits
        println("System resource limits:");
        new Ulimit(this.shell).output(args);
    }

    public static void main(String[] args) throws Exception {
        new SysInfo().processCommand(args);
    }
}
