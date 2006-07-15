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

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.ProcExe;
import org.hyperic.sigar.SigarNotImplementedException;

public class TestProcExe extends SigarTestCase {

    public TestProcExe(String name) {
        super(name);
    }

    private void printExe(Sigar sigar, long pid) throws SigarException {
        traceln("\npid=" + pid);        

        try {
            ProcExe exe = sigar.getProcExe(pid);

            String cwd = exe.getCwd();
            traceln("cwd='" + cwd + "'");

            //assertTrue(new File(cwd).isDirectory());

            traceln("exe='" + exe.getName() + "'");

            //assertTrue(new File(exeFile).exists());
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcExe(getInvalidPid());
        } catch (SigarException e) {
        }

        try {
            ProcExe exe = sigar.getProcExe(sigar.getPid());

            File exeFile = new File(exe.getName());
            String cwd = exe.getCwd();
            traceln("cwd='" + cwd + "'");

            if (cwd.length() > 0) {
                assertTrue(new File(cwd).isDirectory());
            }

            traceln("exe='" + exe.getName() + "'");

            if (exe.getName().length() > 0) {
                assertTrue(exeFile.exists());

                //win32 has .exe
                assertTrue(exeFile.getName().startsWith("java"));
            }
        } catch (SigarNotImplementedException e) {
            //ok
        }

	long[] pids = sigar.getProcList();

        //go through all just to make sure no crashes
	for (int i=0; i<pids.length; i++) {
            try {
                printExe(sigar, pids[i]);
            } catch (SigarException e) {
            } catch (junit.framework.AssertionFailedError e) {
            }
	}
    }
}
