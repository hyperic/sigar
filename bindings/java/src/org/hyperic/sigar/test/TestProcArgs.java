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
import org.hyperic.sigar.SigarNotImplementedException;

public class TestProcArgs extends SigarTestCase {

    public TestProcArgs(String name) {
        super(name);
    }

    private boolean findArg(String[] args, String what) {
        boolean found = false;

        traceln("find=" + what);

        for (int i=0; i<args.length; i++) {
            traceln("   " + i + "=" + args[i]);

            if (args[i].equals(what)) {
                found = true;
            }
        }

        return found;
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcArgs(getInvalidPid());
        } catch (SigarException e) {
        }

        try {
            String[] args = sigar.getProcArgs(sigar.getPid());

            if (getVerbose()) {
                findArg(args, TestProcArgs.class.getName());
            }

            if (args.length > 0) {
                assertTrue(args[0].indexOf("java") != -1);
            }

            //hpux has a limit less than what these args will be
            if (!System.getProperty("os.name").equals("HP-UX")) {
                //and this test only works when run under ant
                //assertTrue(findArg(args, TestProcArgs.class.getName()));
            }
        } catch (SigarNotImplementedException e) {
            //ok; might not happen on win32
        }

	long[] pids = sigar.getProcList();

	for (int i=0; i<pids.length; i++) {
            String pidTrace = "pid=" + pids[i];
            try {
                String[] args = sigar.getProcArgs(pids[i]);
                traceln(pidTrace);
                for (int j=0; j<args.length; j++) {
                    traceln("   " + j + "=>" + args[j] + "<==");
                }
            } catch (SigarException e) {
                traceln(pidTrace + ": " + e.getMessage());
            }
	}
    }
}
