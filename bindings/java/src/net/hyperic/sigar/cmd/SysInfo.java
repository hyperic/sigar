package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Mem;
import net.hyperic.sigar.Swap;
import net.hyperic.sigar.SigarException;

import java.io.File;
import java.io.FilenameFilter;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;

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

    public void output(String[] args) throws SigarException {
        String osName = System.getProperty("os.name");

        /**
         * OS info
         */
        this.out.println(osName + " " +
            System.getProperty("os.version") + " " +
            System.getProperty("os.arch"));

        /**
         * craziness to try to determine the linux distro
         */
        if (osName.equals("Linux")) {
            File etc = new File("/etc");
            File[] release = etc.listFiles(new FilenameFilter()
            {
                public boolean accept(File dir, String name)
                {
                    boolean distroFile =
                        name.toLowerCase().matches(".*elease.*");
                    if (!distroFile) {
                        distroFile = name.toLowerCase().matches(".*ersion.*");
                    }
                    return distroFile;
                }
            });

            if (release.length != 1) {
                this.out.println("Cannot determine Linux distribution!");
                if (release.length > 1) {
                    this.out.println("Possible version files:");
                    for (int i = 0 ; i < release.length ; i++) {
                        this.out.println("  " + release[i]);
                    }
                }
            }
            else {
                try {
                    //this.out.println(release[0].getAbsoluteFile() + ":");
                    BufferedReader rel = new BufferedReader(
                        new FileReader(release[0]));
                    String line;
                    while ((line = rel.readLine()) != null) {
                        this.out.println(line);
                    }
                }
                catch (IOException e) {
                    this.out.println("Cannot determine Linux distribution!");
                }
            }
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
