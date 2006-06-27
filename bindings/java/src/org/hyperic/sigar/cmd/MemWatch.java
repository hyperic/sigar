package org.hyperic.sigar.cmd;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.ProcMem;

/**
 * Watch for changes in program memory usage.
 */
public class MemWatch {

    static final int SLEEP_TIME = 1000 * 10;

    public static void main(String[] args) throws Exception {
        Sigar sigar = new Sigar();

        if (args.length != 1) {
            throw new Exception("Usage: MemWatch pid");
        }

        long pid = Long.parseLong(args[0]);

        long lastTime = System.currentTimeMillis();

        ProcMem last = sigar.getProcMem(pid);

        while (true) {
            ProcMem cur = sigar.getProcMem(pid);
            
            StringBuffer diff = diff(last, cur);

            if (diff.length() == 0) {
                System.out.println("no change " +
                                   "(size=" +
                                   Sigar.formatSize(cur.getSize()) +
                                   ")");
            }
            else {
                long curTime = System.currentTimeMillis();
                long timeDiff = curTime - lastTime;
                lastTime = curTime;
                diff.append(" after " + timeDiff + "ms");
                System.out.println(diff);
            }

            last = cur;
            Thread.sleep(SLEEP_TIME);
        }
    }

    private static StringBuffer diff(ProcMem last, ProcMem cur) {
        StringBuffer buf = new StringBuffer();

        long diff;

        diff = cur.getSize() - last.getSize();
        if (diff != 0) {
            buf.append("size=" + diff);
        }

        diff = cur.getResident() - last.getResident();
        if (diff != 0) {
            buf.append(", resident=" + diff);
        }

        diff = cur.getShare() - last.getShare();
        if (diff != 0) {
            buf.append(", share=" + diff);
        }

        return buf;
    }
}

            
