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

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public abstract class FileWatcher {

    private Sigar sigar;
    private long interval = 0;
    private long lastTime = 0;
    private Set files =
        Collections.synchronizedSet(new HashSet());

    public abstract void onChange(FileInfo info);

    public void onNotFound(FileInfo info) {
    }

    public void onException(FileInfo info, SigarException e) {
    }

    public FileWatcher(Sigar sigar) {
        this.sigar = sigar;
    }

    public void setInterval(long interval) {
        this.interval = interval;
    }

    public long getInterval() {
        return this.interval;
    }

    public FileInfo add(File file) 
        throws SigarException {
        return add(file.getAbsolutePath());
    }

    public FileInfo add(String file)
        throws SigarException {
        FileInfo info = this.sigar.getFileInfo(file);
        this.files.add(info);
        return info;
    }

    public void add(File[] files)
        throws SigarException {
        for (int i=0; i<files.length; i++) {
            add(files[i]);
        }
    }

    public void add(String[] files)
        throws SigarException {
        for (int i=0; i<files.length; i++) {
            add(files[i]);
        }
    }
    
    public void remove(File file) {
        remove(file.getAbsolutePath());
    }

    public void remove(String file) {
        FileInfo info = new FileInfo();
        info.name = file;
        this.files.remove(info);
    }

    public void clear() {
        this.files.clear();
    }

    public Set getFiles() {
        return this.files;
    }

    protected boolean changed(FileInfo info)
        throws SigarException,
               SigarFileNotFoundException {

        return info.changed();
    }

    public void check() {
        if (this.interval != 0) {
            long timeNow = System.currentTimeMillis();
            long timeDiff = timeNow - this.lastTime;

            if (timeDiff < this.interval) {
                return;
            }

            this.lastTime = timeNow;
        }

        synchronized (this.files) {
            for (Iterator it = this.files.iterator();
                 it.hasNext();)
            {
                FileInfo info = (FileInfo)it.next();

                try {
                    if (changed(info)) {
                        this.onChange(info);
                    }
                } catch (SigarFileNotFoundException e) {
                    this.onNotFound(info);
                } catch (SigarException e) {
                    this.onException(info, e);
                }
            }
        }
    }
}
