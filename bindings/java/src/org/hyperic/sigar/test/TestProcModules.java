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

import java.util.List;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;

public class TestProcModules extends SigarTestCase {

    public TestProcModules(String name) {
        super(name);
    }

    private void printModules(Sigar sigar, long pid) throws SigarException {
        traceln("\npid=" + pid);

        try {
            List modules = sigar.getProcModules(pid);

            for (int i=0; i<modules.size(); i++) {
                traceln(i + "=" + modules.get(i));
            }
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
	    printModules(sigar, getInvalidPid());
        } catch (SigarException e) {
        }

        try {
	    printModules(sigar, sigar.getPid());
        } catch (SigarNotImplementedException e) {
            return;
        }

	long[] pids = sigar.getProcList();

	for (int i=0; i<pids.length; i++) {
            try {
                printModules(sigar, pids[i]);
            } catch (SigarException e) {
                traceln(pids[i] + ": " + e.getMessage());
            }
	}
    }
}
