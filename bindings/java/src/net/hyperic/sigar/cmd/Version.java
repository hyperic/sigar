package net.hyperic.sigar.cmd;

import java.io.PrintStream;

import net.hyperic.sigar.Sigar;

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

    public static void printInfo(PrintStream os) {
        os.println("Sigar version=" + Sigar.VERSION_STRING);
        os.println("Sigar build date=" + Sigar.BUILD_DATE);
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
