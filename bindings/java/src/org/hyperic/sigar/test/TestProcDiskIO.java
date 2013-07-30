package org.hyperic.sigar.test;

import org.hyperic.sigar.ProcDiskIO;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;

public class TestProcDiskIO extends SigarTestCase {

	public TestProcDiskIO(String name) {
		super(name);
	}
	
	
    private void traceDiskIO(Sigar sigar, long pid) throws Exception {
        ProcDiskIO procDiskIO;

        try {
            procDiskIO = sigar.getProcDiskIO(pid);
        } catch (SigarException e) {
            traceln("pid " + pid + ": " + e.getMessage());
            return;
        }

        traceln("Pid=" + pid);
        traceln("Bytes Read=" + Sigar.formatSize(procDiskIO.getBytesRead()));
        traceln("Bytes Written=" + Sigar.formatSize(procDiskIO.getBytesWritten()));
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        try {
            sigar.getProcDiskIO(getInvalidPid());
        } catch (SigarException e) {
        }

        long[] pids = sigar.getProcList();
        for (int i=0; i<pids.length; i++) {
            traceDiskIO(sigar, pids[i]);
        }
    }

}
