package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Mem;
import net.hyperic.sigar.Swap;
import net.hyperic.sigar.SigarException;

/**
 * Display amount of free and used memory in the system.
 */
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

    private static Long format(long value) {
        return new Long(value / 1024);
    }

    public void output(String[] args) throws SigarException {
        Mem mem   = this.sigar.getMem();
        Swap swap = this.sigar.getSwap();

        Object[] header = new Object[] { "total", "used", "free" };

        Object[] memRow = new Object[] {
            format(mem.getTotal()),
            format(mem.getUsed()),
            format(mem.getFree())
        };

        Object[] swapRow = new Object[] {
            format(swap.getTotal()),
            format(swap.getUsed()),
            format(swap.getFree())
        };

        printf("%18s %10s %10s", header);

        printf("Mem:    %10ld %10ld %10ld", memRow);

        printf("Swap:   %10ld %10ld %10ld", swapRow);

        printf("RAM:    %10ls", new Object[] { mem.getRam() + "MB" });
    }

    public static void main(String[] args) throws Exception {
        new Free().processCommand(args);
    }
}
