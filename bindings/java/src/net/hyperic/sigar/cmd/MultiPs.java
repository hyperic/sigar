package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.CpuPerc;
import net.hyperic.sigar.MultiProcCpu;

/**
 * Show multi process status.
 */
public class MultiPs extends SigarCommandBase {

    public MultiPs(Shell shell) {
        super(shell);
    }

    public MultiPs() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length == 1;
    }

    public String getSyntaxArgs() {
        return "query";
    }

    public String getUsageShort() {
        return "Show multi process status";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        String query = args[0];
        MultiProcCpu cpu = this.proxy.getMultiProcCpu(query);
        println("Number of processes: " + cpu.getProcesses());
        println("Cpu usage: " + CpuPerc.format(cpu.getPercent()));
        println("Cpu time: "  + Ps.getCpuTime(cpu));
    }

    public static void main(String[] args) throws Exception {
        new MultiPs().processCommand(args);
    }
}

            
