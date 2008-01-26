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

import org.hyperic.jni.ArchLoader;
import org.hyperic.jni.ArchLoaderException;
import org.hyperic.jni.ArchName;
import org.hyperic.jni.ArchNotSupportedException;

public class SigarLoader extends ArchLoader {
    public static final String PROP_SIGAR_JAR_NAME = "sigar.jar.name";

    private static String location = null;
    private static String nativeName = null;

    public SigarLoader(Class loaderClass) {
        super(loaderClass);
    }

    //XXX same as super.getArchLibName()
    //but db2monitor.jar gets loaded first in jboss
    //results in NoSuchMethodError
    public String getArchLibName()
        throws ArchNotSupportedException {

        return getName() + "-" + ArchName.getName();
    }

    public String getDefaultLibName()
        throws ArchNotSupportedException {

        return getArchLibName(); //drop "java" prefix
    }

    //override these methods to ensure our ClassLoader
    //loads the native library.
    protected void systemLoadLibrary(String name) {
        System.loadLibrary(name);
    }

    protected void systemLoad(String name) {
        System.load(name);
    }

    public String getJarName() {
        return System.getProperty(PROP_SIGAR_JAR_NAME,
                                  super.getJarName());
    }

    public static void setSigarJarName(String jarName) {
        System.setProperty(PROP_SIGAR_JAR_NAME, jarName);
    }

    public static String getSigarJarName() {
        return System.getProperty(PROP_SIGAR_JAR_NAME);
    }

    /**
     * Returns the path where sigar.jar is located.
     */
    public synchronized static String getLocation() {
        if (location == null) {
            SigarLoader loader = new SigarLoader(Sigar.class);
            try {
                location = loader.findJarPath(getSigarJarName());
            } catch (ArchLoaderException e) {
                location = ".";
            }
        }
        return location;
    }

    /**
     * Returns the name of the native sigar library.
     */
    public synchronized static String getNativeLibraryName() {
        if (nativeName == null) {
            SigarLoader loader = new SigarLoader(Sigar.class);

            try {
                nativeName = loader.getLibraryName();
            } catch (ArchNotSupportedException e) {
                nativeName = null;
            }
        }

        return nativeName;
    }
}
