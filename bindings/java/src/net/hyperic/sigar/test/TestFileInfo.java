package net.hyperic.sigar.test;

import java.io.IOException;
import java.io.File;
import java.io.FileOutputStream;
import java.util.Date;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarNotImplementedException;
import net.hyperic.sigar.DirStat;
import net.hyperic.sigar.FileInfo;

public class TestFileInfo extends SigarTestCase {

    public TestFileInfo(String name) {
        super(name);
    }

    private void getFileInfo(Sigar sigar, String file) 
        throws SigarException {

        traceln("Entry=" + file);

        FileInfo info = sigar.getFileInfo(file);

        assertGtEqZeroTrace("Permisions",
                            info.getPermissions());

        assertTrueTrace("Permissions",
                        info.getPermissionsString());

        assertGtEqZeroTrace("Mode", info.getMode());

        assertTrueTrace("Type", info.getTypeString());

        assertGtEqZeroTrace("Size", info.getSize());

        assertGtEqZeroTrace("Uid", info.getUid());

        assertGtEqZeroTrace("Gid", info.getUid());

        assertGtEqZeroTrace("Inode", info.getInode());

        traceln("Device=" + info.getDevice());

        assertGtEqZeroTrace("Nlink", info.getNlink());

        assertGtEqZeroTrace("Atime", info.getAtime());
        traceln(new Date(info.getAtime()).toString());

        assertGtZeroTrace("Mtime", info.getMtime());
        traceln(new Date(info.getMtime()).toString());

        assertGtZeroTrace("Ctime", info.getCtime());
        traceln(new Date(info.getCtime()).toString());

        if (info.getType() == FileInfo.TYPE_DIR) {
            try {
                DirStat stats = sigar.getDirStat(file);
                assertEqualsTrace("Total",
                                  new File(file).list().length,
                                  stats.getTotal());
                assertGtEqZeroTrace("Files", stats.getFiles());
                assertGtEqZeroTrace("Subdirs", stats.getSubdirs());
            } catch (SigarNotImplementedException e) {
                //XXX win32
            }
        }
        else {
            try {
                sigar.getDirStat(file);
                assertTrue(false);
            } catch (SigarException e) {
                assertTrue(true);
            }
        }

        sigar.getLinkInfo(file);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        String file;
        File dir = new File(System.getProperty("user.dir"));
        String[] entries = dir.list();
        
        for (int i=0; i<entries.length; i++) {
            file = entries[i];
            File testFile = new File(dir, file);
            if (!(testFile.exists() && testFile.canRead())) {
                continue;
            }
            if (testFile.isHidden()) {
                continue;
            }
            traceln(file + ":");
            getFileInfo(sigar,
                        testFile.getAbsolutePath());
        }

        file = "NO SUCH FILE";

        try {
            getFileInfo(sigar, file);
            assertTrue(false);
        } catch (SigarNotImplementedException e) {
            //XXX win32
        } catch (SigarException e) {
            traceln(file + ": " + e.getMessage());
            assertTrue(true);
        }

        File tmp = File.createTempFile("sigar-", "");
        file = tmp.getAbsolutePath();
        tmp.deleteOnExit();
        traceln("TMP=" + file);

        try {
            //stat() mtime is in seconds, this happens to quick to detect change.
            Thread.sleep(1000 * 1);
        } catch (InterruptedException e) {
        }

        try {
            FileInfo info = sigar.getFileInfo(file);

            FileOutputStream os = null;
            try {
                os = new FileOutputStream(file);
                os.write(1);
            } catch (IOException ioe) {
                throw ioe;
            } finally {
                if (os != null) {
                    try { os.close(); } catch (IOException e) { }
                }
            }

            tmp.setReadOnly();
 
            boolean changed = info.changed();
            traceln(info.diff());
            assertTrue(info.getPreviousInfo().getSize() != info.getSize());
            assertTrue(changed);
        } catch (SigarNotImplementedException e) {
            //XXX win32
        } catch (SigarException e) {
            throw e;
        }
    }
}
