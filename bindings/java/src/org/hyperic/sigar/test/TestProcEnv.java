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

import java.io.File;
import java.util.Map;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.SigarPermissionDeniedException;

public class TestProcEnv extends SigarTestCase {

    public TestProcEnv(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcEnv(getInvalidPid());
        } catch (SigarException e) {
        }

        long pid = sigar.getPid();

        try {
            Map env = sigar.getProcEnv(pid);
            traceln(env.toString());

            String key = "JAVA_HOME";
            String val = (String)env.get(key);

            String single = sigar.getProcEnv(pid, key);

            if (val != null) {
                assertTrue(new File(val, "bin").exists());
                assertTrue(val.equals(single));
                traceln(key + "==>" + single);
            }

            key = "dOeSnOtExIsT";
            val = (String)env.get(key);
            assertTrue(val == null);

            val = sigar.getProcEnv(pid, key);
            assertTrue(val == null);
        } catch (SigarNotImplementedException e) {
            //ok
        } catch (SigarPermissionDeniedException e) {
            //ok
        }

	long[] pids = sigar.getProcList();

	for (int i=0; i<pids.length; i++) {
            //traceln("pid=" + pids[i]);
            try {
                sigar.getProcEnv(pids[i]);
            } catch (SigarException e) {
            }
        }
    }
}
