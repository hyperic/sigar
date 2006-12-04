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

package org.hyperic.sigar.vmware;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import java.util.Arrays;

public class VMControlLibrary {
    public static final String PROP_VMCONTROL_SHLIB =
        "vmcontrol.shlib";

    private static final String VMWARE_LIB =
        getProperty("lib.vmware", "/usr/lib/vmware");

    private static final String VMCONTROL_TAR =
        getProperty("control.tar", VMWARE_LIB + "/perl/control.tar");

    private static final String VMCONTROL = "vmcontrol";

    private static final String VMCONTROL_OBJ =
        getProperty("vmcontrol.o", "control-only/" + VMCONTROL + ".o");

    private static final String GCC =
        getProperty("bin.gcc", "/usr/bin/gcc");

    private static final String TAR =
        getProperty("bin.tar", "/bin/tar");

    private static final String LIBSSL =
        getProperty("libssl", "libssl.so.0.9.7");

    private static String getProperty(String key, String defval) {
        return System.getProperty("vmcontrol." + key, defval);
    }

    private static File getLibSSL() {
        File libssl = new File(VMWARE_LIB, "lib/" + LIBSSL);
        if (libssl.isDirectory()) {
            libssl = new File(libssl, LIBSSL);
        }
        return libssl;
    }

    private static void exec(String[] args)
        throws IOException {

        Process proc = Runtime.getRuntime().exec(args);
        try {
            int exitVal = proc.waitFor();
            if (exitVal != 0) {
                String msg =
                    "exec" + Arrays.asList(args) +
                    " failed: " + exitVal;
                throw new IOException(msg);
            }
        } catch (InterruptedException e) {
        }
    }

    public static String getSharedLibrary() {
        return System.getProperty(PROP_VMCONTROL_SHLIB);
    }

    public static void setSharedLibrary(String lib) {
        System.setProperty(PROP_VMCONTROL_SHLIB, lib);
    }

    public static void link()
        throws IOException {

        link(VMCONTROL + ".so");
    }

    public static void link(String name)
        throws IOException {

        File out = new File(name).getAbsoluteFile();
        setSharedLibrary(out.getPath());

        if (out.exists()) {
            return; //already linked
        }

        File dir = out.getParentFile();
        if (!(dir.isDirectory() && dir.canWrite())) {
            throw new IOException("Cannot write to: " + dir);
        }

        File libssl = getLibSSL();

        if (!libssl.exists()) {
            throw new FileNotFoundException(libssl.toString());
        }

        if (!new File(dir, VMCONTROL_OBJ).exists()) {
            //extract vmcontrol.o
            String[] extract_args = {
                TAR,
                "-xf",
                VMCONTROL_TAR,
                "-C", dir.toString(),
                VMCONTROL_OBJ
            };

            exec(extract_args);
        }

        //create vmcontrol.so from vmcontrol.o
        String[] link_args = {
            GCC,
            "-shared",
            "-o", name,
            VMCONTROL_OBJ,
            libssl.getPath()
        };

        exec(link_args);
    }

    public static boolean isLoaded() {
        return VMwareObject.LOADED;
    }

    public static void main(String[] args) throws Exception {
        link(args[0]);
    }
}
