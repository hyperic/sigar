package net.hyperic.sigar.cmd;

import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarPermissionDeniedException;

/**
 * Display all process information.
 */
public class ProcInfo extends SigarCommandBase {

    public ProcInfo(Shell shell) {
        super(shell);
    }

    public ProcInfo() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display all process info";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        long[] pids = this.shell.findPids(args);

        for (int i=0; i<pids.length; i++) {
            try {
                output(pids[i]);
            } catch (SigarPermissionDeniedException e) {
                println(this.shell.getUserDeniedMessage(pids[i]));
            } catch (SigarException e) {
                println("(" + e.getMessage() + ")");
            }
            println("\n------------------------\n");
        }
    }

    public void output(long pid) throws SigarException {
        println("pid=" + pid);
        try {
            println("state=" + sigar.getProcState(pid));
        } catch (SigarException e) {}
        try {
            println("mem=" + sigar.getProcMem(pid));
        } catch (SigarException e) {}
        try {
            println("time=" + sigar.getProcTime(pid));
        } catch (SigarException e) {}
        try {
            println("cred=" + sigar.getProcCred(pid));
        } catch (SigarException e) {}
        try {
            println("credname=" + sigar.getProcCredName(pid));
        } catch (SigarException e) {}
    }

    public static void main(String[] args) throws Exception {
        new ProcInfo().processCommand(args);
    }
}
