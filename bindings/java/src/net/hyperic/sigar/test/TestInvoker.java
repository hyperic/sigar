package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarLoader;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarProxyCache;
import net.hyperic.sigar.jmx.SigarInvokerJMX;

public class TestInvoker extends SigarTestCase {

    private static final String[][] OK_QUERIES = {
        { "sigar:Type=Mem", "Free" },
        { "sigar:Type=Mem", "Total" },
        { "sigar:Type=Cpu", "User" },
        { "sigar:Type=Cpu", "Sys" },
        { "sigar:Type=CpuPerc", "User" },
        { "sigar:Type=CpuPerc", "Sys" },
        { "sigar:Type=Swap", "Free" },
        { "sigar:Type=Swap", "Used" },
        { "sigar:Type=Uptime", "Uptime" },
        { "sigar:Type=LoadAverage", "0" },
        { "sigar:Type=LoadAverage", "1" },
        { "sigar:Type=LoadAverage", "2" },
        { "sigar:Type=ProcMem,Arg=$$", "Size" },
        { "sigar:Type=ProcMem,Arg=$$", "Vsize" },
        { "sigar:Type=ProcTime,Arg=$$", "Stime" },
        { "sigar:Type=ProcTime,Arg=$$", "Utime" },
        { "sigar:Type=CpuPercList,Arg=0", "Idle" },
    };

    private static final String[][] BROKEN_QUERIES = {
        { "sigar:Type=BREAK", "Free" },
        { "sigar:Type=Mem", "BREAK" },
        { "sigar:Type=ProcTime,Arg=BREAK", "Stime" },
        { "sigar:Type=CpuPercList,Arg=1000", "Idle" },
        { "sigar:Type=CpuPercList,Arg=BREAK", "Idle" },
    };

    public TestInvoker(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        SigarProxy proxy =
            SigarProxyCache.newInstance(sigar);

        traceln("");

        testOK(proxy);
    }

    private void testOK(SigarProxy proxy) throws Exception {
        for (int i=0; i<OK_QUERIES.length; i++) {
            String[] query = OK_QUERIES[i];
            SigarInvokerJMX invoker =
                SigarInvokerJMX.getInstance(proxy, query[0]);
            if (SigarLoader.IS_WIN32 &&
                invoker.getType().equals("LoadAverage"))
            {
                /* XXX not implemented on win32
                 * invoke should throw SigarNotImplementedException
                 */
                continue;
            }
            try {
                Object o = invoker.invoke(query[1]);
                traceln(query[0] + ":" + query[1] + "=" + o);
                assertTrue(true);
            } catch (SigarException e) {
                assertTrue(false);
            }
        }

        for (int i=0; i<BROKEN_QUERIES.length; i++) {
            String[] query = BROKEN_QUERIES[i];
            SigarInvokerJMX invoker =
                SigarInvokerJMX.getInstance(proxy, query[0]);
            try {
                Object o = invoker.invoke(query[1]);
                assertTrue(false);
            } catch (SigarException e) {
                traceln(query[0] + ":" + query[1] + "=" + e.getMessage());
                assertTrue(true);
            }
        }
    }
}
