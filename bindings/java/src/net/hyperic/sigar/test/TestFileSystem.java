package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.FileSystem;
import net.hyperic.sigar.FileSystemMap;
import net.hyperic.sigar.FileSystemUsage;

public class TestFileSystem extends SigarTestCase {

    public TestFileSystem(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = new Sigar();

        FileSystem[] fslist = sigar.getFileSystemList();
        FileSystemMap mounts = sigar.getFileSystemMap();

        String dir = System.getProperty("user.home");
        assertTrueTrace("\nMountPoint for " + dir,
                        mounts.getMountPoint(dir).getDirName());

        for (int i=0; i<fslist.length; i++) {
            FileSystem fs = fslist[i];

            traceln("");
            assertTrue(mounts.getFileSystem(fs.getDirName()) != null);
            assertLengthTrace("DevName", fs.getDevName());
            assertLengthTrace("DirName", fs.getDirName());
            assertLengthTrace("TypeName", fs.getTypeName());
            assertLengthTrace("SysTypeName", fs.getSysTypeName());

            switch (fs.getType()) {
              case FileSystem.TYPE_LOCAL_DISK:
                FileSystemUsage usage =
                    sigar.getFileSystemUsage(fs.getDirName());

                assertGtZeroTrace("  Total", usage.getTotal());
                //possible machines have full filesystems
                assertGtEqZeroTrace("  Free", usage.getFree());
                assertGtEqZeroTrace("  Avail", usage.getAvail());
                double usePercent = usage.getUsePercent() * 100;
                traceln("Used=" + usePercent + "%");
                assertTrue(usePercent <= 100.0);
                break;
            }
        }

        try {
            sigar.getFileSystemUsage("T O T A L L Y B O G U S");
            assertTrue(false);
        } catch (SigarException e) {
            assertTrue(true);
        }
    }
}
