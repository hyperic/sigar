package net.hyperic.sigar;

public class CurrentProcessSummary {
    private int total=0, sleeping=0, running=0, zombie=0, stopped=0;

    private CurrentProcessSummary() { }

    public static CurrentProcessSummary get(SigarProxy sigar)
        throws SigarException {
        
        long[] pids = sigar.getProcList();
        CurrentProcessSummary summary =
            new CurrentProcessSummary();

        for (int i=0; i<pids.length; i++) {
            ProcState state;

            try {
                state = sigar.getProcState(pids[i]);
            } catch (SigarException e) {
                continue; //e.g. stale pid
            }
            
            summary.total++;

            switch (state.getState()) {
              case ProcState.RUN:
                summary.running++;
                break;
              case ProcState.STOP:
                summary.stopped++;
                break;
              case ProcState.SLEEP:
                summary.sleeping++;
                break;
              case ProcState.ZOMBIE:
                summary.zombie++;
                break;
            }
        }

        return summary;
    }

    public int getTotal() {
        return this.total;
    }

    public int getSleeping() {
        return this.sleeping;
    }

    public int getRunning() {
        return this.running;
    }

    public int getZombie() {
        return this.zombie;
    }

    public int getStopped() {
        return this.stopped;
    }

    public String toString() {
        return 
            this.total    + " processes: " +
            this.sleeping + " sleeping, " +
            this.running  + " running, " + 
            this.zombie   + " zombie, " +
            this.stopped  + " stopped";
    }
}
