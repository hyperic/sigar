package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.ThreadCpu;

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
        ThreadCpu cpu = new ThreadCpu();
        long start = System.currentTimeMillis();

        this.shell.handleCommand("time " + args[0], args);

        cpu.gather(this.sigar, 0);
        System.out.println("real....." +
                           (System.currentTimeMillis() - start) / 1000);
        System.out.println("user....." + toMillis(cpu.getUser()));
        System.out.println("sys......" + toMillis(cpu.getSys()));
        System.out.println("total...." + toMillis(cpu.getTotal()));
    }

    private static long toMillis(long nano) {
        return nano / 1000000;
    }

    public static void main(String[] args) throws Exception {
        new Time().processCommand(args);
    }
}
