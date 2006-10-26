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

import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.HashMap;

public abstract class FileTail extends FileWatcher {

    public static final String PROP_USE_SUDO =
        "sigar.tail.sudo";

    private boolean useSudo = 
        "true".equals(System.getProperty(PROP_USE_SUDO));

    private HashMap offsets = new HashMap();
    
    public abstract void tail(FileInfo info, Reader reader);

    public FileTail(Sigar sigar) {
        super(sigar);
    }

    public void useSudo(boolean useSudo) {
        this.useSudo = useSudo;
    }

    private void error(String name, Throwable exc) {
        String msg = name + ": " + exc.getMessage(); 
        SigarLog.getLogger(FileTail.class.getName()).error(msg, exc);
    }

    public void onChange(FileInfo info) {
        Reader reader = null;
        String name = info.getName();

        try {
            if (this.useSudo) {
                reader =
                    new InputStreamReader(new SudoFileInputStream(name));
            }
            else {
                reader = new FileReader(name);
            }
            reader.skip(getOffset(info));
            tail(info, reader);
            setOffset(info);
        } catch (IOException e) {
            error(name, e);
        } finally {
            if (reader != null) {
                try { reader.close(); } catch (IOException e) { }
            }
        }
    }

    public FileInfo add(String file)
        throws SigarException {
        FileInfo info = super.add(file);
        setOffset(info);
        return info;
    }

    protected boolean changed(FileInfo info)
        throws SigarException,
               SigarFileNotFoundException {

        return info.modified();
    }

    private long getOffset(FileInfo info) {
        Long offset = (Long)this.offsets.get(info);

        return offset.longValue();
    }

    private void setOffset(FileInfo info) {
        this.offsets.put(info, new Long(info.size));
    }
}
