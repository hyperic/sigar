package net.hyperic.sigar.cmd;

import java.util.List;

import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarNotImplementedException;

/**
 * Display process module info.
 */
public class ProcModuleInfo extends SigarCommandBase {

    public ProcModuleInfo(Shell shell) {
        super(shell);
    }

    public ProcModuleInfo() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display process module info";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        long[] pids = this.shell.findPids(args);

        for (int i=0; i<pids.length; i++) {
            try {
                output(pids[i]);
            } catch (SigarException e) {
                println("(" + e.getMessage() + ")");
            }
            println("\n------------------------\n");
        }
    }

    public void output(long pid) throws SigarException {
        println("pid=" + pid);

        try {
            List modules = this.sigar.getProcModules(pid);

            for (int i=0; i<modules.size(); i++) {
                println(i + "=" + modules.get(i));
            }
        } catch (SigarNotImplementedException e) {
            throw e;
        } catch (SigarException e) {
            println("[" + e.getMessage() + "]");
        }
    }

    public static void main(String[] args) throws Exception {
        new ProcModuleInfo().processCommand(args);
    }
}
