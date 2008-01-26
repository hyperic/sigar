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
import java.io.FileInputStream;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarLoader;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.SigarPermissionDeniedException;

public class TestProcFd extends SigarTestCase {

    public TestProcFd(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcFd(getInvalidPid());
        } catch (SigarException e) {
        }

        try {
            long pid = sigar.getPid();

            long total = sigar.getProcFd(pid).getTotal(); 

            SigarLoader loader = new SigarLoader(Sigar.class);
            String path = loader.findJarPath(null);
            File file = new File(path, loader.getJarName());

            traceln("Opening " + file);

            FileInputStream is = new FileInputStream(file);

            assertEqualsTrace("Total", total + 1,
                              sigar.getProcFd(pid).getTotal());

            is.close();

            assertEqualsTrace("Total", total,
                              sigar.getProcFd(pid).getTotal());
        } catch (SigarNotImplementedException e) {
            //ok
        } catch (SigarPermissionDeniedException e) {
            //ok
        }
    }
}
