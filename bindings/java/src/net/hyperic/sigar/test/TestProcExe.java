package net.hyperic.sigar.test;

import java.io.File;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.ProcExe;
import net.hyperic.sigar.SigarNotImplementedException;

public class TestProcExe extends SigarTestCase {

    public TestProcExe(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            ProcExe exe = sigar.getProcExe(sigar.getPid());

            File exeFile = new File(exe.getName());
            traceln("exe='" + exe.getName() + "'");

            assertTrue(exeFile.exists());

            //win32 has .exe
            assertTrue(exeFile.getName().startsWith("java"));

            String cwd = exe.getCwd();
            traceln("cwd='" + cwd + "'");

            //XXX win32 as exe but not cwd
            if (cwd.length() != 0) {
                assertTrue(new File(cwd).isDirectory());
            }
        } catch (SigarNotImplementedException e) {
            //ok
        }
    }
}
