package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.ptql.ProcessFinder;

class PidFinder {

    public static long[] getPids(SigarProxy sigar, String[] args)
        throws SigarException {

        long[] pids;

        switch (args.length) {
          case 0:
            pids = new long[] { sigar.getPid() };
            break;
          case 1:
            if (args[0].indexOf("=") > 0) {
                pids = ProcessFinder.find(sigar, args[0]);
            }
            else if (args[0].equals("$$")) {
                pids = new long[] { sigar.getPid() };
            }
            else {
                pids = new long[] {
                    Long.parseLong(args[0])
                };
            }
            break;
          default:
            throw new IllegalArgumentException("Usage: cmd [pid|query]");
        }

        return pids;
    }

    public static long getPid(SigarProxy sigar, String[] args)
        throws SigarException {

        long[] pids = getPids(sigar, args);
        if (pids.length != 1) {
            throw new IllegalArgumentException("Query matches more than 1 process");
        }

        return pids[0];
    }
}
