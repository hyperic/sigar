package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.CpuTimer;

public class Time extends SigarCommandBase {

    public Time(Shell shell) {
        super(shell);
    }

    public Time() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length >= 1;
    }

    public String getSyntaxArgs() {
        return "[command] [...]";
    }

    public String getUsageShort() {
        return "Time command";
    }

    public void output(String[] args) throws SigarException {
        boolean isInteractive = this.shell.isInteractive();
        //turn off paging.
        this.shell.setInteractive(false);
        CpuTimer cpu = new CpuTimer(this.sigar);
        cpu.start();

        try {
            this.shell.handleCommand("time " + args[0], args);
        } finally {
            this.shell.setInteractive(isInteractive);
        }

        cpu.stop();
        cpu.list(this.out);
    }

    public static void main(String[] args) throws Exception {
        new Time().processCommand(args);
    }
}
