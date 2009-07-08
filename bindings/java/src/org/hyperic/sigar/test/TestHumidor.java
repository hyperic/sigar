/*
 * Copyright (C) [2004-2009], Hyperic, Inc.
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

import java.util.ArrayList;

import org.hyperic.sigar.CpuPerc;
import org.hyperic.sigar.Humidor;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;

public class TestHumidor extends SigarTestCase {

    public TestHumidor(String name) {
        super(name);
    }

    private static class HumidorThread extends Thread {
        private SigarProxy sigar;
        private HumidorThread(SigarProxy sigar) {
            this.sigar = sigar;
        }

        public void run() {
            try {
                getProcCpu(this.sigar);
            } catch (Exception e) {
                throw new IllegalArgumentException(e.getMessage());
            }
        }
    }

    private static void getProcCpu(SigarProxy sigar) throws Exception {
        long[] pids = sigar.getProcList();
        for (int j=0; j<10; j++) {
            for (int i=0; i<pids.length; i++) {
                try {
                    double cpu = sigar.getProcCpu(pids[i]).getPercent();
                    if (SigarTestCase.getVerbose()) {
                        System.out.println(Thread.currentThread().getName() + 
                                           " " + pids[i] + "=" + CpuPerc.format(cpu));
                    }
                } catch (SigarException e) {
                    //ok - process may have gone away or permission denied.
                }
            }
        }
    }

    private void runTests(SigarProxy sigar) throws Exception {
        ArrayList threads = new ArrayList();
        for (int i=0; i<3; i++) {
            Thread t = new HumidorThread(sigar);
            threads.add(t);
            t.start();
        }
        for (int i=0; i<threads.size(); i++) {
            Thread t = (Thread)threads.get(i);
            t.join();
        }
    }

    public void testGlobalInstance() throws Exception {
        runTests(Humidor.getInstance().getSigar());
    }

    public void testInstance() throws Exception {
        Sigar sigar = new Sigar();
        runTests(new Humidor(sigar).getSigar());
        sigar.close();
    }

    //uncomment to see if this test will indeed cause a segfault
    //without the protection of the Humidor
    //public void testUnwrapped() throws Exception {
    //    runTests(new Sigar());
    //}
}
