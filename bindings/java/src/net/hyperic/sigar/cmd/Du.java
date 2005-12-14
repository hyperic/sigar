package net.hyperic.sigar.cmd;

import net.hyperic.sigar.DirUsage;
import net.hyperic.sigar.SigarException;

/**
 * Display usage for a directory recursively
 */
public class Du extends SigarCommandBase {

    public Du(Shell shell) {
        super(shell);
    }

    public Du() {
        super();
    }

    public String getUsageShort() {
        return "Display usage for a directory recursively";
    }

    protected boolean validateArgs(String[] args) {
        return args.length == 1;
    }

    public void output(String[] args) throws SigarException {
        String dir = args[0];
        DirUsage du = this.sigar.getDirUsage(dir);
        println(du.getDiskUsage() + "\t" + dir);
    }

    public static void main(String[] args) throws Exception {
        new Du().processCommand(args);
    }
}
