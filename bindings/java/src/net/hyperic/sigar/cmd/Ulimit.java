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

    protected boolean validateArgs(String[] args) {
        return true;
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

        this.mode = "Cur";
        this.invoker =
            SigarInvokerJMX.getInstance(this.proxy, "Type=ResourceLimit");

        for (int i=0; i<args.length; i++) {
            String arg = args[i];
            if (arg.equals("-H")) {
                this.mode = "Max";
            }
            else if (arg.equals("-S")) {
                this.mode = "Cur";
            }
            else {
                throw new SigarException("Unknown argument: " + arg);
            }
        }
        
        println("core file size......." + getValue("Core"));
        println("data seg size........" + getValue("Data"));
        println("file size............" + getValue("FileSize"));
        println("pipe size............" + getValue("PipeSize"));
        println("max memory size......" + getValue("Memory"));
        println("open files..........." + getValue("OpenFiles"));
        println("stack size..........." + getValue("Stack"));
        println("cpu time............." + getValue("Cpu"));
        println("max user processes..." + getValue("Processes"));
        println("virtual memory......." + getValue("VirtualMemory"));
    }

    public static void main(String[] args) throws Exception {
        new Ulimit().processCommand(args);
    }
}
