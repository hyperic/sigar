/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

package org.hyperic.sigar;

/**
 * This class provides a summary of current process states.
 * @see org.hyperic.sigar.cmd.Top
 */
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
