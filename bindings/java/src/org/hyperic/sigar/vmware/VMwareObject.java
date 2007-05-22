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

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarLoader;

abstract class VMwareObject {
    public static final boolean LOADED;

    int ptr = 0;
    long ptr64 = 0;

    private static native boolean init(String lib);

    static {
        LOADED = loadLibraries();
    };

    private static boolean loadLibraries() {
        if (!(SigarLoader.IS_LINUX || SigarLoader.IS_WIN32)) {
            return false;
        }

        try {
            Sigar.load();
            String lib =
                VMControlLibrary.getSharedLibrary();

            if (lib == null) {
                return false;
            }

            if (SigarLoader.IS_WIN32) {
                File root = new File(lib).getParentFile();
                String[] libs = {
                    "libeay32.dll",
                    "ssleay32.dll"
                };

                for (int i=0; i<libs.length; i++) {
                    File ssllib =
                        new File(root, libs[i]);
                    if (!ssllib.exists()) {
                        return false;
                    }

                    try {
                        System.load(ssllib.getPath());
                    } catch (UnsatisfiedLinkError e) {
                        e.printStackTrace();
                        return false;
                    }
                }
            }

            return init(lib);
        } catch (Exception e) {
            //e.printStackTrace();
            return false;
        }
    }

    abstract void destroy();

    public void dispose() {
        if ((this.ptr != 0) || (this.ptr64 != 0)) {
            destroy();
            this.ptr = 0;
            this.ptr64 = 0;
        }
    }

    protected void finalize() {
        dispose();
    }
}
