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

package org.hyperic.sigar.cmd;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.FileInfo;
import org.hyperic.sigar.FileTail;
import org.hyperic.sigar.FileWatcherThread;

/**
 * Display the last part of files to the standard output.
 */
public class Tail {

    public boolean follow;
    public int number = 10;
    public List files = new ArrayList();

    public void parseArgs(String args[]) throws SigarException {
        for (int i=0; i<args.length; i++) {
            String arg = args[i];
            if (arg.charAt(0) != '-') {
                this.files.add(arg);
                continue;
            }
            arg = arg.substring(1);
            if (arg.equals("f")) {
                this.follow = true;
            }
            else if (Character.isDigit(arg.charAt(0))) {
                this.number = Integer.parseInt(arg);
            }
            else {
                throw new SigarException("Unknown argument: " + args[i]);
            }
        }
    }

    public static void main(String[] args) throws SigarException {
        Sigar sigar = new Sigar();

        FileWatcherThread watcherThread = 
            FileWatcherThread.getInstance();

        watcherThread.doStart();

        watcherThread.setInterval(1000);

        FileTail watcher =
            new FileTail(sigar) {
                public void tail(FileInfo info, Reader reader) {
                    String line;
                    BufferedReader buffer =
                        new BufferedReader(reader);

                    if (getFiles().size() > 1) {
                        System.out.println("==> " +
                                           info.getName() +
                                           " <==");
                    }

                    try {
                        while ((line = buffer.readLine()) != null) {
                            System.out.println(line);
                        }
                    } catch (IOException e) {
                        System.out.println(e);                    
                    }
                }
            };

        for (int i=0; i<args.length; i++) {
            watcher.add(args[i]);
        }

        watcherThread.add(watcher);

        try {
            System.in.read();
        } catch (IOException e) { }

        watcherThread.doStop();
    }
}
