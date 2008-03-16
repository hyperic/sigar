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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class OperatingSystem extends SysInfo {

    public static final String NAME_LINUX   = "Linux";
    public static final String NAME_SOLARIS = "Solaris";
    public static final String NAME_HPUX    = "HPUX";
    public static final String NAME_AIX     = "AIX";
    public static final String NAME_MACOSX  = "MacOSX";
    public static final String NAME_FREEBSD = "FreeBSD";
    public static final String NAME_OPENBSD = "OpenBSD";
    public static final String NAME_NETBSD  = "NetBSD";
    public static final String NAME_WIN32   = "Win32";
    public static final String NAME_NETWARE = "NetWare";
    
    public static final String[] UNIX_NAMES = {
        OperatingSystem.NAME_LINUX,
        OperatingSystem.NAME_SOLARIS,
        OperatingSystem.NAME_HPUX,
        OperatingSystem.NAME_AIX,
        OperatingSystem.NAME_MACOSX,
        OperatingSystem.NAME_FREEBSD,
        OperatingSystem.NAME_OPENBSD,
        OperatingSystem.NAME_NETBSD,
    };
        
    public static final String[] WIN32_NAMES = {
        OperatingSystem.NAME_WIN32,
    };

    public static final String[] NAMES;
    
    public static final boolean IS_WIN32 =
        System.getProperty("os.name").indexOf("Windows") != -1;
    
    private static final Map supportedPlatforms = new HashMap();
    
    static {
        int len = UNIX_NAMES.length + WIN32_NAMES.length;
        String[] all = new String[len];
        System.arraycopy(UNIX_NAMES, 0, all, 0, UNIX_NAMES.length);
        all[len-1] = NAME_WIN32;
        NAMES = all;
        
        for (int i=0; i<NAMES.length; i++) {
            supportedPlatforms.put(NAMES[i], Boolean.TRUE);
        }
    }

    public static boolean isSupported(String name) {
        return supportedPlatforms.get(name) == Boolean.TRUE;
    }

    public static boolean isWin32(String name) {
        return OperatingSystem.NAME_WIN32.equals(name);
    }
    
    private static OperatingSystem instance = null;

    private String dataModel;
    private String cpuEndian;
    
    private OperatingSystem() {
    }

    public static synchronized OperatingSystem getInstance() {
        if (instance == null) {
            Sigar sigar = new Sigar();
            OperatingSystem os = new OperatingSystem();
            try {
                os.gather(sigar);
            } catch (SigarException e) {
                throw new IllegalStateException(e.getMessage());
            } finally {
                sigar.close();
            }
            Properties props = System.getProperties();
            os.dataModel = props.getProperty("sun.arch.data.model");
            os.cpuEndian = props.getProperty("sun.cpu.endian");

            instance = os;
        }

        return instance;
    }

    public String getDataModel() {
        return this.dataModel;
    }

    public String getCpuEndian() {
        return this.cpuEndian;
    }

    public static void main(String[] args) {
        System.out.println("all.............." + Arrays.asList(NAMES));
        OperatingSystem os = OperatingSystem.getInstance();
        System.out.println("description......" + os.getDescription());
        System.out.println("name............." + os.name);
        System.out.println("version.........." + os.version);
        System.out.println("arch............." + os.arch);
        System.out.println("patch level......" + os.patchLevel);
        System.out.println("vendor..........." + os.vendor);
        System.out.println("vendor name......" + os.vendorName);
        System.out.println("vendor version..." + os.vendorVersion);
    }
}
