package net.hyperic.sigar.test;

import java.io.File;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.ProcExe;
import net.hyperic.sigar.SigarNotImplementedException;

public class TestProcExe extends SigarTestCase {

    public TestProcExe(String name) {
        super(name);
    }

    private void printExe(Sigar sigar, long pid) throws SigarException {
        traceln("\npid=" + pid);        

        try {
            ProcExe exe = sigar.getProcExe(pid);

            String cwd = exe.getCwd();
            traceln("cwd='" + cwd + "'");

            //assertTrue(new File(cwd).isDirectory());

            File exeFile = new File(exe.getName());
            traceln("exe='" + exe.getName() + "'");

            //assertTrue(exeFile.exists());
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcExe(getInvalidPid());
        } catch (SigarException e) {
        }

        try {
            ProcExe exe = sigar.getProcExe(sigar.getPid());

            File exeFile = new File(exe.getName());
            String cwd = exe.getCwd();
            traceln("cwd='" + cwd + "'");

            if (cwd.length() > 0) {
                assertTrue(new File(cwd).isDirectory());
            }

            traceln("exe='" + exe.getName() + "'");

            assertTrue(exeFile.exists());

            //win32 has .exe
            assertTrue(exeFile.getName().startsWith("java"));
        } catch (SigarNotImplementedException e) {
            //ok
        }

	long[] pids = sigar.getProcList();

        //go through all just to make sure no crashes
	for (int i=0; i<pids.length; i++) {
            try {
                printExe(sigar, pids[i]);
            } catch (SigarException e) {
            } catch (junit.framework.AssertionFailedError e) {
            }
	}
    }
}
