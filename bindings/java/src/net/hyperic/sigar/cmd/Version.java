package net.hyperic.sigar.cmd;

import java.io.PrintStream;
import java.net.InetAddress;
import java.net.UnknownHostException;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;

import net.hyperic.sigar.SigarLoader;
/**
 * Display Sigar, java and system version information.
 */
public class Version extends SigarCommandBase {
    private static String[] SYS_PROPS = {
        "os.name",
        "os.version",
        "os.arch",
        "java.vm.version",
        "java.vm.vendor",
        "sun.arch.data.model",
        "sun.cpu.endian",
        "sun.os.patch.level",
    };

    public Version(Shell shell) {
        super(shell);
    }

    public Version() {
        super();
    }

    public String getUsageShort() {
        return "Display sigar version";
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
        os.println("Sigar version=" + Sigar.VERSION_STRING);
        os.println("Sigar build date=" + Sigar.BUILD_DATE);
        os.println("Sigar archlib=" +
                   SigarLoader.getNativeLibraryName());
        os.println("Current fqdn=" + fqdn);
        if (!fqdn.equals(host)) {
            os.println("Current host=" + host);
        }
        os.println("");

        for (int i=0; i<SYS_PROPS.length; i++) {
            String prop = SYS_PROPS[i];
            os.println(prop + "=" + 
                       System.getProperty(prop));
        }
    }

    public void output(String[] args) {
        printInfo(this.out);
    }

    public static void main(String[] args) throws Exception {
        new Version().processCommand(args);
    }
}
