package net.hyperic.sigar.cmd;

import net.hyperic.sigar.OperatingSystem;
import net.hyperic.sigar.SigarException;

/**
 * Display System Information
 */
public class SysInfo extends SigarCommandBase {

    public SysInfo(Shell shell) {
        super(shell);
    }

    public SysInfo() {
        super();
    }

    public String getUsageShort() {
        return "Display system information";
    }

    private static boolean contains(String value, String substr) {
        //matches does not work w/ jre 1.3
        //value.toLowerCase().matches(".*" + substr + ".*");
        return value.toLowerCase().indexOf(substr) != -1;
    }

    public void output(String[] args) throws SigarException {
        String osName = System.getProperty("os.name");

        /**
         * OS info
         */
        OperatingSystem os = OperatingSystem.getInstance();

        this.out.println(os.getName() + " " +
                         os.getVersion() + " " +
                         os.getArch());

        if (osName.equals("Linux")) {
            println(os.getVendor() + " " + os.getVendorVersion());
        }

        /**
         * JVM info
         */
        this.out.println(System.getProperty("java.vm.name") + " - " +
            System.getProperty("java.vm.vendor") + " [" +
            System.getProperty("java.vm.version") + "]");
        this.out.println(System.getProperty("java.specification.vendor") + " " +
            System.getProperty("java.specification.name") + " " +
            System.getProperty("java.specification.version"));
        this.out.println("JAVA_HOME=" + System.getProperty("java.home"));
        this.out.println("");

        /**
         * uptime
         */
        new Uptime(this.shell).output(args);
        this.out.println("");

        /**
         * CPU info
         */
        net.hyperic.sigar.CpuInfo[] infos =
            this.sigar.getCpuInfoList();
        for (int i=0; i<infos.length; i++) {
            net.hyperic.sigar.CpuInfo info = infos[i];
            this.out.println("CPU " + i + ":");
            this.out.println("  Vendor........" + info.getVendor());
            this.out.println("  Model........." + info.getModel());
            this.out.println("  Mhz..........." + info.getMhz());
            this.out.println("  Cache size...." + info.getCacheSize());
        }
        this.out.println("");

        /**
         * memory info
         */
        new Free(this.shell).output(args);
    }

    public static void main(String[] args) throws Exception {
        new SysInfo().processCommand(args);
    }
}
