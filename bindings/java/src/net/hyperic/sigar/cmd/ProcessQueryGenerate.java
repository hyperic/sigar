package net.hyperic.sigar.cmd;

import net.hyperic.sigar.SigarException;

import net.hyperic.sigar.ptql.ProcessQueryGenerator;

public class ProcessQueryGenerate extends SigarCommandBase {

    public ProcessQueryGenerate(Shell shell) {
        super(shell);
    }

    public ProcessQueryGenerate() {
        super();
    }

    public boolean validateArgs(String[] args) {
        return true;
    }

    public void output(String[] args) throws SigarException {
        ProcessQueryGenerator generator =
            new ProcessQueryGenerator(this.proxy);

        long[] pids;

        if (args.length > 0) {
            pids = this.shell.findPids(args);
        }
        else {
            pids = this.proxy.getProcList();
        }

        for (int i=0; i<pids.length; i++) {
            long pid = pids[i];
            String query = generator.generate(pid);

            if (query != null) {
                println(query);
            }
            else {
                this.err.println("failed to narrow query for " + pid +
                                 " (" +
                                 this.proxy.getProcState(pid).getName() +
                                 ")");
            }
        }

        flush();
    }

    public static void main(String[] args) throws Exception {
        new ProcessQueryGenerate().processCommand(args);
    }
}
