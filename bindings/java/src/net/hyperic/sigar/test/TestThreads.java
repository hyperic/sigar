package net.hyperic.sigar.test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import java.util.ArrayList;

import junit.framework.TestCase;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarProxyCache;
import net.hyperic.sigar.test.Proxy;

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
            int expire = 1000;

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
