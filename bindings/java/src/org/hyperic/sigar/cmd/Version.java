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
import java.io.PrintStream;
import java.net.InetAddress;
import java.net.UnknownHostException;

import org.hyperic.sigar.OperatingSystem;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarLoader;

import org.hyperic.sigar.win32.LocaleInfo;

/**
 * Display Sigar, java and system version information.
 */
public class Version extends SigarCommandBase {

    public Version(Shell shell) {
        super(shell);
    }

    public Version() {
        super();
    }

    public String getUsageShort() {
        return "Display sigar and system version info";
    }

    private static String getHostName() {
        try {
            return InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            return "unknown";
        }
    }

    private static void printNativeInfo(PrintStream os) {
        String version =
            "java=" + Sigar.VERSION_STRING +
            ", native=" + Sigar.NATIVE_VERSION_STRING;
        String build =
            "java=" + Sigar.BUILD_DATE +
            ", native=" + Sigar.NATIVE_BUILD_DATE;
        String scm =
            "java=" + Sigar.SCM_REVISION +
            ", native=" + Sigar.NATIVE_SCM_REVISION;
        String archlib =
            SigarLoader.getNativeLibraryName();

        os.println("Sigar version......." + version);
        os.println("Build date.........." + build);
        os.println("SCM rev............." + scm);
        String host = getHostName();
        String fqdn;
        Sigar sigar = new Sigar(); 
        try {
            File lib = sigar.getNativeLibrary();
            if (lib != null) {
                archlib = lib.getName();
            }
            fqdn = sigar.getFQDN();
        } catch (SigarException e) {
            fqdn = "unknown";
        } finally {
            sigar.close();
        }

        os.println("Archlib............." + archlib);

        os.println("Current fqdn........" + fqdn);
        if (!fqdn.equals(host)) {
            os.println("Hostname............" + host);
        }        

        if (SigarLoader.IS_WIN32) {
            LocaleInfo info = new LocaleInfo();
            os.println("Language............" + info);
            os.println("Perflib lang id....." +
                       info.getPerflibLangId());
        }
    }
    
    public static void printInfo(PrintStream os) {
        try {
            printNativeInfo(os);
        } catch (UnsatisfiedLinkError e) {
            os.println("*******ERROR******* " + e);
        }

        os.println("Current user........" +
                   System.getProperty("user.name"));
        os.println("");
        
        OperatingSystem sys = OperatingSystem.getInstance();
        os.println("OS description......" + sys.getDescription());
        os.println("OS name............." + sys.getName());
        os.println("OS arch............." + sys.getArch());
        os.println("OS machine.........." + sys.getMachine());
        os.println("OS version.........." + sys.getVersion());
        os.println("OS patch level......" + sys.getPatchLevel());
        os.println("OS vendor..........." + sys.getVendor());
        os.println("OS vendor version..." + sys.getVendorVersion());
        if (sys.getVendorCodeName() != null) {
            os.println("OS code name........" + sys.getVendorCodeName());
        }
        os.println("OS data model......." + sys.getDataModel());
        os.println("OS cpu endian......." + sys.getCpuEndian());

        os.println("Java vm version....." + 
                   System.getProperty("java.vm.version"));
        os.println("Java vm vendor......" + 
                System.getProperty("java.vm.vendor"));
        os.println("Java home..........." +
                System.getProperty("java.home"));
    }

    public void output(String[] args) {
        printInfo(this.out);
    }

    public static void main(String[] args) throws Exception {
        new Version().processCommand(args);
    }
}
