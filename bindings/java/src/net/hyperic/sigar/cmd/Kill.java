package net.hyperic.sigar.cmd;

import net.hyperic.sigar.SigarException;

public class Kill extends SigarCommandBase {

    public Kill(Shell shell) {
        super(shell);
    }

    public Kill() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length == 1 || args.length == 2;
    }

    public String getSyntaxArgs() {
        return "[signal] <query|pid>";
    }

    public String getUsageShort() {
        return "Send signal to a process";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        int signum = 15; //SIGTERM
        long[] pids;
        String query;

        if (args.length == 2) {
            query = args[1];
            try {
                signum = Integer.parseInt(args[0]);
            } catch (NumberFormatException e) {
                //XXX convert SIGFOO to number
                throw new SigarException(e.getMessage());
            }
        }
        else {
            query = args[0];
        }

        pids = this.shell.findPids(new String[] { query });

        for (int i=0; i<pids.length; i++) {
            println("kill " + signum + " " + pids[i]);
            this.sigar.kill(pids[i], signum);
        }
    }

    public static void main(String[] args) throws Exception {
        new Kill().processCommand(args);
    }
}
