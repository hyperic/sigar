package net.hyperic.sigar.cmd;

import net.hyperic.sigar.ResourceLimit;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.jmx.SigarInvokerJMX;

/**
 * Display system resource limits.
 */
public class Ulimit extends SigarCommandBase {

    private SigarInvokerJMX invoker;
    private String mode;
    
    public Ulimit(Shell shell) {
        super(shell);
    }

    public Ulimit() {
        super();
    }

    public String getUsageShort() {
        return "Display system resource limits";
    }

    private static String format(long val) {
        if (val == ResourceLimit.INFINITY()) {
            return "unlimited";
        }
        else {
            return String.valueOf(val);
        }
    }

    private String getValue(String attr)
        throws SigarException {
        Long val = (Long)this.invoker.invoke(attr + this.mode);
        return format(val.longValue());
    }
    
    public void output(String[] args) throws SigarException {

        this.invoker =
            SigarInvokerJMX.getInstance(this.proxy, "Type=ResourceLimit");

        this.mode = "Cur";
        
        println("core file size......." + getValue("Core"));
        println("data seg size........" + getValue("Data"));
        println("file size............" + getValue("FileSize"));
        println("max memory size......" + getValue("Memory"));
        println("open files..........." + getValue("OpenFiles"));
        println("stack size..........." + getValue("Stack"));
        println("cpu time............." + getValue("Cpu"));
        println("max user processes..." + getValue("Processes"));
        println("virual memory........" + getValue("VirtualMemory"));
    }

    public static void main(String[] args) throws Exception {
        new Ulimit().processCommand(args);
    }
}
