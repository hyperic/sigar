package net.hyperic.sigar.cmd;

import java.util.Date;
import java.text.SimpleDateFormat;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;

public class Who extends SigarCommandBase {

    public Who(Shell shell) {
        super(shell);
    }

    public Who() {
        super();
    }

    public String getUsageShort() {
        return "Show who is logged on";
    }

    private String getTime(long time) {
        String fmt = "MMM dd HH:mm";
        return new SimpleDateFormat(fmt).format(new Date(time));
    }

    public void output(String[] args) throws SigarException {
        net.hyperic.sigar.Who[] who = this.sigar.getWhoList();
        for (int i=0; i<who.length; i++) {
            println(who[i].getUser() + "\t" +
                    who[i].getDevice() + "\t" +
                    getTime(who[i].getTime() * 1000) + "\t" +
                    who[i].getHost());
        }
    }

    public static void main(String[] args) throws Exception {
        new Who().processCommand(args);
    }
}
