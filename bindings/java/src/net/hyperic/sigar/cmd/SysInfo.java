package net.hyperic.sigar.cmd;

import net.hyperic.sigar.CpuInfo;
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

    private String getJavaProp(String name) {
        return System.getProperty("java." + name);
    }

    public void output(String[] args) throws SigarException {
        /**
         * OS info
         */
        OperatingSystem os = OperatingSystem.getInstance();

        println(os.getName() + " " +
                os.getVersion() + " " +
                os.getArch());

        if (os.getName().equals("Linux")) {
            println(os.getVendor() + " " + os.getVendorVersion());
        }

        /**
         * JVM info
         */
        println(getJavaProp("vm.name") + " - " +
                getJavaProp("vm.vendor") + " [" +
                getJavaProp("vm.version") + "]");

        println(getJavaProp("specification.vendor") + " " +
                getJavaProp("specification.name") + " " +
                getJavaProp("specification.version"));
        println("JAVA_HOME=" + getJavaProp("home"));
        println("");

        /**
         * uptime
         */
        new Uptime(this.shell).output(args);
        println("");

        /**
         * CPU info
         */
        CpuInfo[] infos = this.sigar.getCpuInfoList();
        for (int i=0; i<infos.length; i++) {
            CpuInfo info = infos[i];
            println("CPU " + i + ":");
            println("  Vendor........" + info.getVendor());
            println("  Model........." + info.getModel());
            println("  Mhz..........." + info.getMhz());
            println("  Cache size...." + info.getCacheSize());
        }
        println("");

        /**
         * memory info
         */
        new Free(this.shell).output(args);
    }

    public static void main(String[] args) throws Exception {
        new SysInfo().processCommand(args);
    }
}
