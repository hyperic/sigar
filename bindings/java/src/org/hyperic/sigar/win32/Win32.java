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

package org.hyperic.sigar.win32;

import java.io.File;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;

public abstract class Win32 {

    public static final String EXE_EXT = ".exe";

    static {
        try {
            Sigar.load();
        } catch (SigarException e) {
            
        }
    }

    public static native String findExecutable(String name)
        throws SigarException;

    public static String findScriptExecutable(String name) {
        int ix = name.lastIndexOf(".");
        if (ix == -1) {
            return null;
        }

        String ext = name.substring(ix+1);
        if (ext.equals("exe") ||
            ext.equals("bat") ||
            ext.equals("com"))
        {
            return null;
        }

        String exe;
        try {
            exe = findExecutable(new File(name).getAbsolutePath());
        } catch (SigarException e) {
            return null;
        }
        if (exe == null) {
            return null; //no association
        }

        exe = exe.toLowerCase();
        name = name.toLowerCase();
        if (exe.equals(name) || exe.endsWith(name)) {
            return null; //same thing
        }

        File file = new File(exe);
        //rewrite to use cscript for command line stuff
        if (file.getName().equals("wscript.exe")) {
            exe =
                file.getParent() +
                File.separator +
                "cscript.exe";
        }

        return exe;
    }

    public static FileVersion getFileVersion(String name) {
        FileVersion version = new FileVersion();
        if (version.gather(name)) {
            return version;
        }
        else {
            return null;
        }
    }

    public static void main(String[] args) throws Exception {
        for (int i=0; i<args.length; i++) {
            String file =
                new File(args[i]).getAbsoluteFile().toString();
            String exe =
                findScriptExecutable(file);
            if (exe == null) {
                continue;
            }
            System.out.println(args[i] + "=" + exe);
        }
    }
}
