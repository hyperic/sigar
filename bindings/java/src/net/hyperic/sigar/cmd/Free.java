package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Mem;
import net.hyperic.sigar.Swap;
import net.hyperic.sigar.SigarException;

public class Free extends SigarCommandBase {

    public Free(Shell shell) {
        super(shell);
    }

    public Free() {
        super();
    }

    public String getUsageShort() {
        return "Display information about free and used memory";
    }

    public void output(String[] args) throws SigarException {
        Mem mem = this.sigar.getMem();
        Swap swap = this.sigar.getSwap();

        this.out.println("\tTotal\tUsed\tFree");

        this.out.println("Mem:    " +
                         mem.getTotal() / 1024 + "\t" +
                         mem.getUsed() / 1024 + "\t" +
                         mem.getFree() / 1024);

        this.out.println("Swap:   " + 
                         swap.getTotal() / 1024 + "\t" +
                         swap.getUsed() / 1024 + "\t" +
                         swap.getFree() / 1024);

        this.out.println("RAM:    " + mem.getRam() + "MB");        
    }

    public static void main(String[] args) throws Exception {
        new Free().processCommand(args);
    }
}
