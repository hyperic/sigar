package net.hyperic.sigar.cmd;

import net.hyperic.sigar.ResourceLimit;
import net.hyperic.sigar.SigarException;

/**
 * Display system resource limits.
 */
public class Ulimit extends SigarCommandBase {

    public Ulimit(Shell shell) {
        super(shell);
    }

    public Ulimit() {
        super();
    }

    public String getUsageShort() {
        return "Display system resource limits";
    }

    private static String format(ResourceLimit rlimit, long val) {
        if (val == rlimit.getUnlimited()) {
            return "unlimited";
        }
        else {
            return String.valueOf(val);
        }
    }

    public void output(String[] args) throws SigarException {
        ResourceLimit rlimit = this.sigar.getResourceLimit();

        println("core file size......." +
                format(rlimit, rlimit.getCoreCur()));
        println("data seg size........" +
                format(rlimit, rlimit.getDataCur()));
        println("file size............" +
                format(rlimit, rlimit.getFileSizeCur()));
        println("max memory size......" +
                format(rlimit, rlimit.getMemoryCur()));
        println("open files..........." +
                format(rlimit, rlimit.getOpenFilesCur()));
        println("stack size..........." +
                format(rlimit, rlimit.getStackCur()));
        println("cpu time............." +
                format(rlimit, rlimit.getCpuCur()));
        println("max user processes..." +
                format(rlimit, rlimit.getProcessesCur()));
        println("virual memory........" +
                format(rlimit, rlimit.getVirtualMemoryCur()));
    }

    public static void main(String[] args) throws Exception {
        new Ulimit().processCommand(args);
    }
}
