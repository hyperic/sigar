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

package org.hyperic.sigar.win32.test;

import java.io.File;

import org.hyperic.sigar.test.SigarTestCase;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.ProcExe;
import org.hyperic.sigar.win32.Win32;
import org.hyperic.sigar.win32.FileVersion;

public class TestFileVersion extends SigarTestCase {

    public TestFileVersion(String name) {
        super(name);
    }

    private void printExe(long pid) {
        traceln("\npid=" + pid);        

        String name;
        try {
            name = getSigar().getProcExe(pid).getName();
        } catch (SigarException e) {
            return;
        }

        FileVersion info = Win32.getFileVersion(name);
        if (info != null) {
            traceln("exe='" + name + "'");
            traceln("version=" + info.getProductVersion());
        }
    }

    public void testCreate() throws Exception {
        assertTrue(Win32.getFileVersion("DoEsNoTeXist.exe") == null);

	long[] pids = getSigar().getProcList();

        //go through all just to make sure no crashes
	for (int i=0; i<pids.length; i++) {
            printExe(pids[i]);
	}
    }
}
