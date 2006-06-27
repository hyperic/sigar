package org.hyperic.sigar.test;

import java.util.List;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;

public class TestProcModules extends SigarTestCase {

    public TestProcModules(String name) {
        super(name);
    }

    private void printModules(Sigar sigar, long pid) throws SigarException {
        traceln("\npid=" + pid);

        try {
            List modules = sigar.getProcModules(pid);

            for (int i=0; i<modules.size(); i++) {
                traceln(i + "=" + modules.get(i));
            }
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
	    printModules(sigar, getInvalidPid());
        } catch (SigarException e) {
        }

        try {
	    printModules(sigar, sigar.getPid());
        } catch (SigarNotImplementedException e) {
            return;
        }

	long[] pids = sigar.getProcList();

	for (int i=0; i<pids.length; i++) {
            try {
                printModules(sigar, pids[i]);
            } catch (SigarException e) {
                traceln(pids[i] + ": " + e.getMessage());
            }
	}
    }
}
