package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarPermissionDeniedException;
import net.hyperic.sigar.ProcFd;
import net.hyperic.sigar.ProcExe;

/**
 * Display process file information.
 */
public class ProcFileInfo extends SigarCommandBase {

    public ProcFileInfo(Shell shell) {
        super(shell);
    }

    public ProcFileInfo() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display process file info";
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
        ProcFd fd = sigar.getProcFd(pid);

        println("pid=" + pid);

        println("open file descriptors=" + fd.getTotal());

        ProcExe exe = sigar.getProcExe(pid);
        String name = exe.getName();
        if (name.length() == 0) {
            name = "unknown";
        }
        println("name=" + name);

        println("cwd=" + exe.getCwd());
    }

    public static void main(String[] args) throws Exception {
        new ProcFileInfo().processCommand(args);
    }
}
