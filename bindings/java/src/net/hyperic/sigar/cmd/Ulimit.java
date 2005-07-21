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

    private static String format(long val) {
        if (val == ResourceLimit.INFINITY()) {
            return "unlimited";
        }
        else {
            return String.valueOf(val);
        }
    }

    public void output(String[] args) throws SigarException {
        ResourceLimit rlimit = this.sigar.getResourceLimit();

        println("core file size......." +
                format(rlimit.getCoreCur()));
        println("data seg size........" +
                format(rlimit.getDataCur()));
        println("file size............" +
                format(rlimit.getFileSizeCur()));
        println("max memory size......" +
                format(rlimit.getMemoryCur()));
        println("open files..........." +
                format(rlimit.getOpenFilesCur()));
        println("stack size..........." +
                format(rlimit.getStackCur()));
        println("cpu time............." +
                format(rlimit.getCpuCur()));
        println("max user processes..." +
                format(rlimit.getProcessesCur()));
        println("virual memory........" +
                format(rlimit.getVirtualMemoryCur()));
    }

    public static void main(String[] args) throws Exception {
        new Ulimit().processCommand(args);
    }
}
