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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import java.util.ArrayList;

import junit.framework.TestCase;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarProxyCache;
import org.hyperic.sigar.test.Proxy;

//test concurrency
public class TestThreads extends TestCase {

    private static Sigar gSigar = null;
    private static SigarProxy gProxy = null;
    private static Object lock = new Object();
    private static boolean verbose = true;

    class ProxyThread extends Thread {
        SigarException ex;
        //true should make things blow up
        boolean useGlobal = false;

        public void run() {
            Sigar sigar;
            SigarProxy proxy;

            try {
                synchronized (lock) {
                    if (useGlobal) {
                        if (gSigar == null) {

                            gSigar = new Sigar();

                            gProxy = SigarProxyCache.newInstance(gSigar, 30 * 1000);
                        }

                        sigar = gSigar;
                        proxy = gProxy;
                    }
                    else {
                        sigar = new Sigar();
                    
                        proxy = SigarProxyCache.newInstance(sigar, 30 * 1000);
                    }
                }

                String args[] = {"leaktest", "50"};
                Proxy cmdProxy = new Proxy(sigar, proxy);

                PrintStream ps = new PrintStream(new ByteArrayOutputStream());

                if (verbose) {
                    cmdProxy.setVerbose(true);
                    cmdProxy.setLeakVerbose(true);
                    cmdProxy.run(args);
                }
                else {
                    cmdProxy.setOutputStream(ps);
                }
            } catch (SigarException e) {
                this.ex = e;
            }
        }
    }

    public TestThreads(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        ArrayList threads = new ArrayList();

        for (int i=0; i<4; i++) {
            ProxyThread pt = new ProxyThread();
            pt.useGlobal = true;
            threads.add(pt);
            pt.start();
        }

        for (int n=0; n<threads.size(); n++) {
            ProxyThread pt = (ProxyThread)threads.get(n);
            pt.join();
            if (pt.ex != null) {
                pt.ex.printStackTrace();
                fail(pt.ex.getMessage());
            }
        }
    }
}
