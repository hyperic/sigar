package net.hyperic.sigar.cmd;

import java.io.PrintStream;
import java.net.InetAddress;
import java.net.UnknownHostException;

import net.hyperic.sigar.OperatingSystem;
import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;

import net.hyperic.sigar.SigarLoader;
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

    private static String getFQDN() {
        try {
            return Sigar.getInstance().getFQDN();
        } catch (SigarException e) {
            return "unknown";
        }
    }

    public static void printInfo(PrintStream os) {
        String fqdn = getFQDN();
        String host = getHostName();
        os.println("Sigar version......." + Sigar.VERSION_STRING);
        os.println("Build date.........." + Sigar.BUILD_DATE);
        os.println("Archlib............." +
                   SigarLoader.getNativeLibraryName());
        os.println("Current fqdn........" + fqdn);
        if (!fqdn.equals(host)) {
            os.println("Hostname............" + host);
        }
        os.println("");
        
        OperatingSystem sys = OperatingSystem.getInstance();
        os.println("OS description......" + sys.getDescription());
        os.println("OS name............." + sys.getName());
        os.println("OS arch............." + sys.getArch());
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
