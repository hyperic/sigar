package net.hyperic.sigar.test;

import java.io.File;
import java.io.FileInputStream;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarLoader;
import net.hyperic.sigar.SigarNotImplementedException;

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
            String sigarJar =
                new SigarLoader(Sigar.class).findJarPath("sigar.jar");

            File file = new File(sigarJar + File.separator + "sigar.jar");

            traceln("Opening " + file);

            FileInputStream is = new FileInputStream(file);

            assertEqualsTrace("Total", total + 1,
                              sigar.getProcFd(pid).getTotal());

            is.close();

            assertEqualsTrace("Total", total,
                              sigar.getProcFd(pid).getTotal());
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }
}
