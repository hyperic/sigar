/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

package org.hyperic.sigar;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import org.apache.log4j.Logger;

//intended for use on systems with very large connection tables
//where processing /proc/net/tcp may block or otherwise take much longer
//than reading a plain 'ol text file
public class ProcFileMirror extends FileWatcher {

    private String proc;
    private long expire;

    private static final Logger log = 
        SigarLog.getLogger(ProcFileMirror.class.getName());

    private static final boolean isDebug = log.isDebugEnabled();

    public ProcFileMirror(Sigar sigar, String proc) {
        super(sigar);

        this.proc = proc;
        this.expire = FileWatcherThread.DEFAULT_INTERVAL;
    }

    public long getExpireMillis() {
        return this.expire;
    }

    public void setExpire(long seconds) {
        setExpireMillis(seconds * 1000);
    }

    public void setExpireMillis(long millis) {
        this.expire = millis;
    }

    public String getProcFile(File file) {
        return getProcFile(file.getPath());
    }

    public String getProcFile(String file) {
        final String PROC = "/proc/";
        if (file.startsWith(PROC)) {
            file = file.substring(PROC.length());
        }
        return this.proc + File.separator + file;
    }

    private void mirror(String source) throws IOException {
        mirror(source, getProcFile(source));
    }

    private String mirrorToString(File source, File dest) {
        return "mirror(" + source + ", " + dest + ")";
    }

    private void mirror(String source, String dest) throws IOException {
        mirror(new File(source), new File(dest));
    }

    private void mirror(File source, File dest) throws IOException {
        FileInputStream is = null;
        FileOutputStream os = null;

        try {
            is = new FileInputStream(source);
            os = new FileOutputStream(dest);
            byte[] buffer = new byte[2048];
            int nread;
            while (true) {
                nread = is.read(buffer);
                if (nread == -1) {
                    break;
                }
                os.write(buffer, 0, nread);
            }
        } catch (IOException e) {
            String msg =
                mirrorToString(source, dest) + " failed: " +
                e.getMessage();
            throw new IOException(msg);
        } finally {
            if (is != null) {
                try { is.close(); } catch (IOException e) {}
            }
            if (os != null) {
                os.close();
            }
        }

        if (isDebug) {
            log.debug(mirrorToString(source, dest));
        }
    }

    public FileInfo add(String name)
        throws SigarException {

        File source = new File(name);
        File dest = new File(getProcFile(source));
        File dir = dest.getParentFile();

        if (!dir.exists()) {
            if (!dir.mkdirs()) {
                String msg = "mkdir(" + dir + ") failed";
                throw new SigarException(msg);
            }
        }
        if (!source.canRead()) {
            throw new SigarException("Cannot read: " + source);
        }
        if ((dest.isFile() && !dest.canWrite()) ||
            !dir.isDirectory() ||
            !dir.canWrite())
        {
            throw new SigarException("Cannot write: " + dest);
        }

        try {
            mirror(source.getPath(), dest.getPath());
        } catch (IOException e) {
            throw new SigarException(e.getMessage());
        }

        return super.add(source.getPath());
    }

    public void onChange(FileInfo info) {
        try {
            mirror(info.getName());
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }

    protected boolean changed(FileInfo info)
        throws SigarException,
               SigarFileNotFoundException {

        File dest = new File(getProcFile(info.getName()));
        long now = System.currentTimeMillis();

        if ((now - dest.lastModified()) > this.expire) {
            return true;
        }
        else {
            return false;
        }
    }
}
