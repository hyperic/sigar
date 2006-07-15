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

package org.hyperic.sigar.test;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.ProcState;

public class TestProcState extends SigarTestCase {

    public TestProcState(String name) {
        super(name);
    }

    private void traceState(Sigar sigar, long pid) {
        try {
            ProcState procState =
                sigar.getProcState(pid);
            traceln("[pid=" + pid + "] " + procState);
        } catch (SigarException e) {
            traceln("pid " + pid + ": " + e.getMessage());
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcState(getInvalidPid());
        } catch (SigarException e) {
        }

        ProcState procState = sigar.getProcState(sigar.getPid());
        traceState(sigar, sigar.getPid());

        char state = procState.getState();
        assertTrue((state == 'R') || (state == 'S'));
        assertTrue(procState.getName().indexOf("java") != -1);

        long[] pids = sigar.getProcList();
        for (int i=0; i<pids.length; i++) {
            traceState(sigar, pids[i]);
        }
    }
}
