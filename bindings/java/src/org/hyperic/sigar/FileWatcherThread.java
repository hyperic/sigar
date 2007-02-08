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

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class FileWatcherThread implements Runnable {

    public static final int DEFAULT_INTERVAL = 60 * 5 * 1000;

    private Thread thread = null;
    private static FileWatcherThread instance = null;
    private boolean shouldDie = false;
    private long interval = DEFAULT_INTERVAL;
    private Set watchers =
        Collections.synchronizedSet(new HashSet());

    public static synchronized FileWatcherThread getInstance() {
        if (instance == null) {
            instance = new FileWatcherThread();
        }

        return instance;
    }

    public synchronized void doStart() {
        if (this.thread != null) {
            return;
        }

        this.thread = new Thread(this, "FileWatcherThread");
        this.thread.setDaemon(true);
        this.thread.start();
    }

    public synchronized void doStop() {
        if (this.thread == null) {
            return;
        }
        die();
        this.thread.interrupt();
        this.thread = null;
    }

    public void setInterval(long interval) {
        this.interval = interval;
    }

    public long getInterval() {
        return this.interval;
    }

    public void add(FileWatcher watcher) {
        this.watchers.add(watcher);
    }

    public void remove(FileWatcher watcher) {
        this.watchers.remove(watcher);
    }

    public void run() {
        while (!shouldDie) {
            check();
            try {
                Thread.sleep(this.interval);
            } catch (InterruptedException e) {
            }
        }
    }

    public void die() {
        this.shouldDie = true;
    }

    public void check() {
        synchronized (this.watchers) {
            for (Iterator it = this.watchers.iterator();
                 it.hasNext();)
            {
                FileWatcher watcher = (FileWatcher)it.next();
                try {
                    watcher.check();
                } catch (Exception e) {
                    FileTail.error("Unexpected exception: " +
                                   e.getMessage(), e);
                }
            }
        }
    }
}
