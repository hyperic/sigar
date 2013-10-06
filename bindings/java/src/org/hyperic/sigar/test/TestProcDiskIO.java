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
            // throw e;
            return;
        }

        long bytesRead = procDiskIO.getBytesRead();
        long bytesWritten = procDiskIO.getBytesWritten();
        long bytesTotal = procDiskIO.getBytesTotal();
        
        traceln("Pid=" + pid);
        traceln("Bytes Read=" + Sigar.formatSize(bytesRead));
        traceln("Bytes Written=" + Sigar.formatSize(bytesWritten));
        traceln("Bytes Total=" + Sigar.formatSize(bytesTotal));
        
        if (bytesRead != -1 && bytesWritten != -1 && bytesTotal != -1) {
        	assertTrue("Bytes total should equal bytesRead + bytesWritten",
        			    (bytesTotal == bytesRead + bytesWritten));
        }
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        boolean caughtException = false;
        try {
            sigar.getProcDiskIO(getInvalidPid());
        } catch (SigarException e) {
        	caughtException = true;
        }
        assertTrue("Test on invalid PID should have thrown an exception.", caughtException);

        long[] pids = sigar.getProcList();
        for (int i=0; i<pids.length; i++) {
            traceDiskIO(sigar, pids[i]);
        }
    }

}
