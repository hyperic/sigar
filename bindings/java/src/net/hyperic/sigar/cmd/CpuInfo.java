package net.hyperic.sigar.cmd;

import net.hyperic.sigar.CpuPerc;
import net.hyperic.sigar.SigarException;

public class CpuInfo extends SigarCommandBase {

    public CpuInfo(Shell shell) {
        super(shell);
    }

    public CpuInfo() {
        super();
    }

    public String getUsageShort() {
        return "Display cpu information";
    }

    public void output(String[] args) throws SigarException {
        net.hyperic.sigar.CpuInfo[] infos =
            this.sigar.getCpuInfoList();

        CpuPerc[] cpus = this.sigar.getCpuPercList();

        this.out.println(infos.length + " total CPUs..");

        for (int i=0; i<infos.length; i++) {
            net.hyperic.sigar.CpuInfo info = infos[i];
            CpuPerc cpu = cpus[i];
            this.out.println("Vendor........" + info.getVendor());
            this.out.println("Model........." + info.getModel());
            this.out.println("Mhz..........." + info.getMhz());
            this.out.println("Cache size...." + info.getCacheSize());
            this.out.println("User Time....." + CpuPerc.format(cpu.getUser()));
            this.out.println("Sys Time......" + CpuPerc.format(cpu.getSys()));
            this.out.println("Idle Time....." + CpuPerc.format(cpu.getIdle()));
            this.out.println();
        }
    }

    public static void main(String[] args) throws Exception {
        new CpuInfo().processCommand(args);
    }
}
