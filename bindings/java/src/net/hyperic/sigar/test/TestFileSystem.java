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
        Sigar sigar = getSigar();

        FileSystem[] fslist = sigar.getFileSystemList();
        FileSystemMap mounts = sigar.getFileSystemMap();

        String dir = System.getProperty("user.home");
        assertTrueTrace("\nMountPoint for " + dir,
                        mounts.getMountPoint(dir).getDirName());

        for (int i=0; i<fslist.length; i++) {
            FileSystem fs = fslist[i];

            assertTrue(mounts.getFileSystem(fs.getDirName()) != null);
            assertLengthTrace("DevName", fs.getDevName());
            assertLengthTrace("DirName", fs.getDirName());
            assertLengthTrace("TypeName", fs.getTypeName());
            assertLengthTrace("SysTypeName", fs.getSysTypeName());

            FileSystemUsage usage;

            try {
                usage = sigar.getFileSystemUsage(fs.getDirName());
            } catch (SigarException e) {
                if (fs.getType() == FileSystem.TYPE_LOCAL_DISK) {
                    throw e;
                }
                //else ok, e.g. floppy drive on windows
                continue;
            }

            switch (fs.getType()) {
              case FileSystem.TYPE_LOCAL_DISK:
                assertGtZeroTrace("  Total", usage.getTotal());
                //possible machines have full filesystems
                assertGtEqZeroTrace("  Free", usage.getFree());
                assertGtEqZeroTrace("  Avail", usage.getAvail());
                double usePercent = usage.getUsePercent() * 100;
                traceln("  Used=" + usePercent + "%");
                assertTrue(usePercent <= 100.0);
              default:
                traceln("  DiskReads=" + usage.getDiskReads());
                traceln("  DiskWrites=" + usage.getDiskWrites());
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
