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
import java.util.ArrayList;
import java.util.StringTokenizer;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;

public abstract class Win32 {

    public static final String EXE_EXT = ".exe";
    private static final String EXE_EXT_U = EXE_EXT.toUpperCase();

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

    public static String[] parseCommandLine(String args) {
        ArrayList res = new ArrayList();
        StringTokenizer quoteTok;
        boolean inQuote = false;

        if ((args == null) ||
            ((args = args.trim()).length() == 0))
        {
            return new String[0];
        }
            
        if (!args.startsWith("\"") &&
            (args.endsWith(EXE_EXT) ||
             args.endsWith(EXE_EXT_U)) &&
            new File(args).exists())
        {
            return new String[] { args };
        }

        quoteTok = new StringTokenizer(args, "\"", true);

        while (quoteTok.hasMoreTokens()) {
            String elem = (String)quoteTok.nextElement();

            if (elem.equals("\"")) {
                inQuote = !inQuote;
                continue;
            }

            if (inQuote) {
                res.add(elem);
            }
            else {
                StringTokenizer spaceTok = new StringTokenizer(elem.trim());

                while (spaceTok.hasMoreTokens()) {
                    res.add(spaceTok.nextToken());
                }
            }
        }
        
        if (inQuote) {
            throw new IllegalArgumentException("Unbalanced quotation marks");
        }

        return (String[])res.toArray(new String[0]);
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
