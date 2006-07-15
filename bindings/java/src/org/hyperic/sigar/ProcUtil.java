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
import java.io.IOException;
import java.util.jar.Attributes;
import java.util.jar.JarFile;

public class ProcUtil {

    private static boolean isClassName(String name) {
        int len = name.length();
        if (len == 0) {
            return false;
        }

        for (int i=0; i<len; i++) {
            char c = name.charAt(i);
            if (!((c == '.') || Character.isLetter(c))) {
                return false;
            }
        }

        return true;
    }

    /**
     * Try to determina classname for java programs
     */
    public static String getJavaMainClass(SigarProxy sigar, long pid)
        throws SigarException {

        String[] args = sigar.getProcArgs(pid);
        for (int i=1; i<args.length; i++) {
            String arg = args[i];
            if (isClassName(arg.trim())) {
                //example: "java:weblogic.Server"
                return arg;
            }
            else if (arg.equals("-jar")) {
                File file = new File(args[i+1]);
                if (!file.isAbsolute()) {
                    try {
                        String cwd =
                            sigar.getProcExe(pid).getCwd();
                        file = new File(cwd + File.separator + file);
                    } catch (SigarException e) {}
                }

                if (file.exists()) {
                    JarFile jar = null;
                    try {
                        jar = new JarFile(file);
                        return
                            jar.getManifest().
                            getMainAttributes().
                            getValue(Attributes.Name.MAIN_CLASS);
                    } catch (IOException e) {
                    } finally {
                        if (jar != null) {
                            try { jar.close(); }
                            catch (IOException e){}
                        }
                    }
                }
                
                return file.toString();
            }
        }

        return null;
    }

    public static String getDescription(SigarProxy sigar, long pid)
        throws SigarException {

        String[] args;
        ProcState state = sigar.getProcState(pid);
        String name = state.getName();

        try {
            args = sigar.getProcArgs(pid);
        } catch (SigarException e) {
            args = new String[0];
        }

        if (name.equals("java") || name.equals("javaw")) {
            String className = null;
            try {
                className = getJavaMainClass(sigar, pid);
            } catch (SigarException e) {}
            if (className != null) {
                name += ":" + className;
            }
        }
        else if (args.length != 0) {
            name = args[0];
        }
        else {
            try {
                String exe =
                    sigar.getProcExe(pid).getName();
                if (exe.length() != 0) {
                    name = exe;
                }
            } catch (SigarException e) {}
        }

        return name;
    }
}
