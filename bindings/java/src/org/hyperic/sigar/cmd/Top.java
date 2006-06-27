package org.hyperic.sigar.cmd;

import java.util.List;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.CpuPerc;
import org.hyperic.sigar.SigarProxyCache;
import org.hyperic.sigar.ProcCpu;
import org.hyperic.sigar.CurrentProcessSummary;

/**
 * Display system resource utilization summaries and process information.
 * <p>
 * This version of the top command requires a ptql query to select which
 * processes to display.
 *
 * Example to display java processes only:<br>
 * <code>% java -jar sigar-bin/lib/sigar.jar Top State.Name.eq=java</code>
 *
 * @see org.hyperic.sigar.ptql.ProcessQueryBuilder
 */
public class Top {
    private static final int SLEEP_TIME = 1000 * 5;

    private static final String HEADER =
        "PID\tUSER\tSTIME\tSIZE\tRSS\tSHARE\tSTATE\tTIME\t%CPU\tCOMMAND";

    public static void main(String[] args) throws Exception {
        Sigar sigarImpl = new Sigar();

        SigarProxy sigar = 
            SigarProxyCache.newInstance(sigarImpl, SLEEP_TIME);

        while (true) {
            Shell.clearScreen();

            System.out.println(Uptime.getInfo(sigar));

            System.out.println(CurrentProcessSummary.get(sigar));

            System.out.println(sigar.getCpuPerc());

            System.out.println(sigar.getMem());

            System.out.println(sigar.getSwap());
                               
            System.out.println();

            System.out.println(HEADER);

            long[] pids = Shell.getPids(sigar, args);

            for (int i=0; i<pids.length; i++) {
                long pid = pids[i];

                String cpuPerc = "?";

                List info = Ps.getInfo(sigar, pid);
                try {
                    ProcCpu cpu = sigar.getProcCpu(pid);
                    cpuPerc = CpuPerc.format(cpu.getPercent());
                } catch (SigarException e) {
                }

                info.add(info.size()-1, cpuPerc);

                System.out.println(Ps.join(info));
            }

            Thread.sleep(SLEEP_TIME);
            SigarProxyCache.clear(sigar);
        }
    }
}
