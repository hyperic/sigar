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

import java.util.ArrayList;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.FileSystem;
import org.hyperic.sigar.FileSystemMap;
import org.hyperic.sigar.FileSystemUsage;
import org.hyperic.sigar.NfsFileSystem;

import org.hyperic.sigar.shell.FileCompleter;
import org.hyperic.sigar.util.GetlineCompleter;

/**
 * Report filesytem disk space usage.
 */
public class Df extends SigarCommandBase {

    private static final String OUTPUT_FORMAT =
        "%-15s %4s %4s %5s %4s %-15s %s";

    //like df -h -a
    private static final String[] HEADER = new String[] {
        "Filesystem",
        "Size",
        "Used",
        "Avail",
        "Use%",
        "Mounted on",
        "Type"
    };

    private GetlineCompleter completer;

    public Df(Shell shell) {
        super(shell);
        setOutputFormat(OUTPUT_FORMAT);
        this.completer = new FileCompleter(shell);
    }

    public Df() {
        super();
        setOutputFormat(OUTPUT_FORMAT);
    }

    public GetlineCompleter getCompleter() {
        return this.completer;
    }

    protected boolean validateArgs(String[] args) {
        return args.length <= 1;
    }

    public String getSyntaxArgs() {
        return "[filesystem]";
    }

    public String getUsageShort() {
        return "Report filesystem disk space usage";
    }

    public void printHeader() {
        printf(HEADER);
    }

    public void output(String[] args) throws SigarException {
        if (args.length == 1) {
            FileSystemMap mounts = this.proxy.getFileSystemMap();
            String name = FileCompleter.expand(args[0]);
            FileSystem fs = mounts.getMountPoint(name);

            if (fs != null) {
                printHeader();
                output(fs);
                return;
            }

            throw new SigarException(args[0] +
                                     " No such file or directory");
        }
        else {
            FileSystem[] fslist = this.proxy.getFileSystemList();
            printHeader();
            for (int i=0; i<fslist.length; i++) {
                output(fslist[i]);
            }
        }
    }

    public void output(FileSystem fs) throws SigarException {
        long used, avail, total, pct;

        try {
            FileSystemUsage usage;
            if (fs instanceof NfsFileSystem) {
                NfsFileSystem nfs = (NfsFileSystem)fs;
                if (!nfs.ping()) {
                    println(nfs.getUnreachableMessage());
                    return;
                }
            }
            usage = this.sigar.getFileSystemUsage(fs.getDirName());

            used = usage.getTotal() - usage.getFree();
            avail = usage.getAvail();
            total = usage.getTotal();

            pct = (long)(usage.getUsePercent() * 100);
        } catch (SigarException e) {
            //e.g. on win32 D:\ fails with "Device not ready"
            //if there is no cd in the drive.
            used = avail = total = pct = 0;
        }

        String usePct;
        if (pct == 0) {
            usePct = "-";
        }
        else {
            usePct = pct + "%";
        }
        
        ArrayList items = new ArrayList();

        items.add(fs.getDevName());
        items.add(formatSize(total));
        items.add(formatSize(used));
        items.add(formatSize(avail));
        items.add(usePct);
        items.add(fs.getDirName());
        items.add(fs.getSysTypeName() + "/" + fs.getTypeName());

        printf(items);
    }

    private static String formatSize(long size) {
        return Sigar.formatSize(size * 1024);
    }

    public static void main(String[] args) throws Exception {
        new Df().processCommand(args);
    }
}
