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

import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import org.apache.log4j.Logger;

public abstract class FileTail extends FileWatcher {

    public static final String PROP_USE_SUDO =
        "sigar.tail.sudo";

    private boolean useSudo = 
        "true".equals(System.getProperty(PROP_USE_SUDO));

    private static final Logger log = 
        SigarLog.getLogger(FileTail.class.getName());

    private static final boolean isDebug = log.isDebugEnabled();

    public abstract void tail(FileInfo info, Reader reader);

    public FileTail(Sigar sigar) {
        super(sigar);
    }

    public void useSudo(boolean useSudo) {
        this.useSudo = useSudo;
    }

    static void error(String name, Throwable exc) {
        String msg = name + ": " + exc.getMessage(); 
        log.error(msg, exc);
    }

    public void onChange(FileInfo info) {
        InputStream in = null;
        Reader reader = null;
        String name = info.getName();

        try {
            if (this.useSudo) {
                in = new SudoFileInputStream(name);
            }
            else {
                in = new FileInputStream(name);
            }

            long offset = getOffset(info);

            if (offset > 0) {
                in.skip(offset); //use InputStream to skip bytes
            }
            reader = new InputStreamReader(in);
            tail(info, reader);
        } catch (IOException e) {
            error(name, e);
        } finally {
            if (reader != null) {
                try { reader.close(); } catch (IOException e) { }
            }
            if (in != null) {
                try { in.close(); } catch (IOException e) { }
            }
        }
    }

    public FileInfo add(String file)
        throws SigarException {
        FileInfo info = super.add(file);
        if (isDebug) {
            log.debug("add: " + file + "=" + info);
        }
        return info;
    }

    protected boolean changed(FileInfo info)
        throws SigarException,
               SigarFileNotFoundException {

        return
            info.modified() ||
            (info.getPreviousInfo().size != info.size);
    }

    private long getOffset(FileInfo current) {
        FileInfo previous = current.getPreviousInfo();

        if (previous == null) {
            if (isDebug) {
                log.debug(current.getName() + ": first stat");
            }
            return current.size;
        }

        if (current.inode != previous.inode) {
            if (isDebug) {
                log.debug(current.getName() + ": file inode changed");
            }
            return -1;
        }

        if (current.size < previous.size) {
            if (isDebug) {
                log.debug(current.getName() + ": file truncated");
            }
            return -1;
        }

        if (isDebug) {
            long diff = current.size - previous.size;
            log.debug(current.getName() + ": " + diff + " new bytes");
        }

        return previous.size;
    }
}
