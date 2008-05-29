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

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.text.SimpleDateFormat;
import org.hyperic.sigar.FileInfo;
import org.hyperic.sigar.SigarException;

public class Ls extends SigarCommandBase {

    public Ls(Shell shell) {
        super(shell);
    }

    public Ls() {
        super();
    }

    public String getUsageShort() {
        return "simple FileInfo test at the moment (like ls -l)";
    }

    protected boolean validateArgs(String[] args) {
        return args.length == 1;
    }

    private String getDate(long mtime) {
        final String fmt = "MMM dd  yyyy";

        return new SimpleDateFormat(fmt).format(new Date(mtime));
    }

    public void output(String[] args) throws SigarException {
        String file = args[0];
        FileInfo link = this.sigar.getLinkInfo(file);
        FileInfo info = this.sigar.getFileInfo(file);
        if (link.getType() == FileInfo.TYPE_LNK) {
            try {
                file = file + " -> " + new File(file).getCanonicalPath();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        println(link.getTypeChar() + 
                info.getPermissionsString() + "\t" +
                info.getUid() + "\t" + info.getGid() + "\t" +
                info.getSize() + "\t" +
                getDate(info.getMtime()) + "\t" +
                file);
    }

    public static void main(String[] args) throws Exception {
        new Ls().processCommand(args);
    }
}
