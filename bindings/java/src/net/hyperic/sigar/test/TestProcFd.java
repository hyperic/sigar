package net.hyperic.sigar.test;

import java.io.File;
import java.io.FileInputStream;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarNotImplementedException;

public class TestProcFd extends SigarTestCase {

    public TestProcFd(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        traceln("");

        try {
            long pid = sigar.getPid();

            long total = sigar.getProcFd(pid).getTotal(); 

            File file = new File("bin", "run_tests.sh");
            if (!file.exists()) {
                file = new File("build.xml");
            }
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
