package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.ProcCredName;
import net.hyperic.sigar.ProcMem;
import net.hyperic.sigar.ProcTime;
import net.hyperic.sigar.ProcState;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Show process status.
 */
public class Ps extends SigarCommandBase {

    public Ps(Shell shell) {
        super(shell);
    }

    public Ps() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getSyntaxArgs() {
        return "[pid|query]";
    }

    public String getUsageShort() {
        return "Show process status";
    }

    public boolean isPidCompleter() {
        return true;
    }

    public void output(String[] args) throws SigarException {
        long[] pids;
        if (args.length == 0) {
            pids = this.proxy.getProcList();
        }
        else {
            pids = this.shell.findPids(args);
        }

        for (int i=0; i<pids.length; i++) {
            long pid = pids[i];
            try {
                output(pid);
            } catch (SigarException e) {
                this.err.println("Exception getting process info for " +
                                 pid + ": " + e.getMessage());
            }
        }
    }

    public static String join(List info) {
        StringBuffer buf = new StringBuffer();
        Iterator i = info.iterator();
        boolean hasNext = i.hasNext();
        while (hasNext) {
            buf.append((String)i.next());
            hasNext = i.hasNext();
            if (hasNext)
                buf.append("\t");
        }

        return buf.toString();
    }

    private static boolean isClassName(String name) {
        int len = name.length();
        for (int i=0; i<len; i++) {
            char c = name.charAt(i);
            if (!((c == '.') || Character.isLetter(c))) {
                return false;
            }
        }

        return true;
    }

    public static List getInfo(SigarProxy sigar, long pid)
        throws SigarException {

        ProcState state = sigar.getProcState(pid);
        ProcTime time = null;
        String unknown = "???";

        List info = new ArrayList();
        info.add(String.valueOf(pid));

        try {
            ProcCredName cred = sigar.getProcCredName(pid);
            info.add(cred.getUser());
        } catch (SigarException e) {
            info.add(unknown);
        }

        try {
            time = sigar.getProcTime(pid);
            info.add(getStartTime(time.getStartTime()));
        } catch (SigarException e) {
            info.add(unknown);
        }

        try {
            ProcMem mem = sigar.getProcMem(pid);
            info.add(Sigar.formatSize(mem.getSize()));
            info.add(Sigar.formatSize(mem.getRss()));
            info.add(Sigar.formatSize(mem.getShare()));
        } catch (SigarException e) {
            info.add(unknown);
        }

        info.add(String.valueOf(state.getState()));

        if (time != null) {
            info.add(getCpuTime(time));
        }
        else {
            info.add(unknown);
        }

        String name = state.getName();
        if (name.equals("java")) {
            //try to guess classname for java programs
            try {
                String[] args = sigar.getProcArgs(pid);
                for (int i=1; i<args.length; i++) {
                    String arg = args[i];
                    if (!isClassName(arg)) {
                        continue;
                    }
                    //example: "java:weblogic.Server"
                    name += ":" + arg;
                    break;
                }

            } catch (SigarException e) {}
        }
        else {
            try {
                String[] args = sigar.getProcArgs(pid);
                name = args[0];
            } catch (SigarException e) {}
        }

        info.add(name);

        return info;
    }

    public void output(long pid) throws SigarException {
        println(join(getInfo(this.proxy, pid)));
    }

    private static String getCpuTime(ProcTime time) {
        long t = (time.getUser() + time.getSys());
        return t/60 + ":" + t%60;
    }

    private static String getStartTime(long time) {
        if (time == 0) {
            return "00:00";
        }
        long timeNow = System.currentTimeMillis();
        String fmt = "MMMd";

        if ((timeNow - time) < ((60*60*24) * 1000)) {
            fmt = "HH:mm";
        }

        return new SimpleDateFormat(fmt).format(new Date(time));
    }

    public static void main(String[] args) throws Exception {
        new Ps().processCommand(args);
    }
}

            
