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

            File file = new File(SigarLoader.getLocation(), "sigar.jar");

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
