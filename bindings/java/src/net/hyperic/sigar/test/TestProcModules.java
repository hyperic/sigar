package net.hyperic.sigar.test;

import java.io.File;
import java.util.List;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarNotImplementedException;

public class TestProcModules extends SigarTestCase {

    public TestProcModules(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        traceln("");

        try {
            List modules = sigar.getProcModules(sigar.getPid());

            for (int i=0; i<modules.size(); i++) {
                traceln(i + "=" + modules.get(i));
            }
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }
}
