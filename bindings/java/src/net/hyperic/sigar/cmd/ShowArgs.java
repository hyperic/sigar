package net.hyperic.sigar.cmd;

import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarNotImplementedException;

/**
 * Show process command line arguments.
 */
public class ShowArgs extends SigarCommandBase {

    public ShowArgs(Shell shell) {
        super(shell);
    }

    public ShowArgs() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Show process command line arguments";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        long[] pids = this.shell.findPids(args);

        for (int i=0; i<pids.length; i++) {
            output(pids[i]);
            println("\n------------------------\n");
        }
    }

    public void output(long pid) throws SigarException {

        String[] argv = this.proxy.getProcArgs(pid);
        
        println("pid=" + pid);

        try {
            String exe = this.proxy.getProcExe(pid).getName();
            println("exe=" + exe);
        } catch (SigarNotImplementedException e) {
        } catch (SigarException e) {
            println("exe=???");
        }

        try {
            String cwd = this.proxy.getProcExe(pid).getCwd();
            println("cwd=" + cwd);
        } catch (SigarNotImplementedException e) {
        } catch (SigarException e) {
            println("cwd=???");
        }

        for (int i=0; i<argv.length; i++) {
            println("   " + i + "=" + argv[i]);
        }
    }

    public static void main(String[] args) throws Exception {
        new ShowArgs().processCommand(args);
    }
}
