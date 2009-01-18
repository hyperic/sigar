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

package org.hyperic.jni;

/**
 * Helper class for naming the jni library with 
 * platform/arch name for which it is binary compatible.
 */
public class ArchName {

    static boolean useDmalloc =
        System.getProperty("jni.dmalloc") != null;

    public static String getName() throws ArchNotSupportedException {
        String name = getArchName();
        if (useDmalloc) {
            name += "-dmalloc";
        }
        return name;
    }

    public static boolean is64() {
        return "64".equals(System.getProperty("sun.arch.data.model"));
    }

    private static String getArchName() throws ArchNotSupportedException {
        String name    = System.getProperty("os.name");
        String arch    = System.getProperty("os.arch");
        String version = System.getProperty("os.version");
        String majorVersion = version.substring(0, 1); //4.x, 5.x, etc.

        if (arch.endsWith("86")) {
            arch = "x86";
        }

        if (name.equals("Linux")) {
            return arch + "-linux";
        }
        else if (name.indexOf("Windows") > -1) {
            return arch + "-winnt";
        }
        else if (name.equals("SunOS")) {
            if (arch.startsWith("sparcv") && is64()) {
                arch = "sparc64";
            }
            return arch + "-solaris";
        }
        else if (name.equals("HP-UX")) {
            if (arch.startsWith("IA64")) {
                arch = "ia64";
            }
            else {
                arch = "pa";
            }
            if (version.indexOf("11") > -1) {
                return arch + "-hpux-11";
            }
        }
        else if (name.equals("AIX")) {
            if (majorVersion.equals("6")) {
                //v5 binary is compatible with v6
                majorVersion = "5";
            }
            //arch == "ppc" on 32-bit, "ppc64" on 64-bit 
            return arch + "-aix-" + majorVersion;
        }
        else if (name.equals("Mac OS X") || name.equals("Darwin")) {
            if (is64()) {
                return "universal64-macosx";
            }
            else {
                return "universal-macosx";
            }
        }
        else if (name.equals("FreeBSD")) {
            //none of the 4,5,6 major versions are binary compatible
            return arch + "-freebsd-" + majorVersion;
        }
        else if (name.equals("OpenBSD")) {
            return arch + "-openbsd-" + majorVersion;
        }
        else if (name.equals("NetBSD")) {
            return arch + "-netbsd-" + majorVersion;
        }
        else if (name.equals("OSF1")) {
            return "alpha-osf1-" + majorVersion;
        }
        else if (name.equals("NetWare")) {
            return "x86-netware-" + majorVersion;
        }

        String desc = arch + "-" + name + "-" + version;

        throw new ArchNotSupportedException("platform (" + desc + ") not supported");
    }

    private ArchName () { }

}
