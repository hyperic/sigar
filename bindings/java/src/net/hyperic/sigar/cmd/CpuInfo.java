package net.hyperic.sigar.cmd;

import net.hyperic.sigar.CpuPerc;
import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;

/**
 * Display cpu information for each cpu found on the system.
 */
public class CpuInfo extends SigarCommandBase {

    public boolean displayTimes = true;
    
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

        CpuPerc[] cpus = null;

        println(infos.length + " total CPUs..");
        net.hyperic.sigar.CpuInfo info = infos[0];
        long cacheSize = info.getCacheSize();
        println("Vendor........" + info.getVendor());
        println("Model........." + info.getModel());
        println("Mhz..........." + info.getMhz());
        if (cacheSize != Sigar.FIELD_NOTIMPL) {
            println("Cache size...." + cacheSize);
        }

        if (!this.displayTimes) {
            return;
        }

        for (int i=0; i<infos.length; i++) {
            if (cpus == null) {
                cpus = this.sigar.getCpuPercList();
            }
            CpuPerc cpu = cpus[i];
            println("User Time....." + CpuPerc.format(cpu.getUser()));
            println("Sys Time......" + CpuPerc.format(cpu.getSys()));
            println("Idle Time....." + CpuPerc.format(cpu.getIdle()));
            println("Wait Time....." + CpuPerc.format(cpu.getWait()));
            println("Nice Time....." + CpuPerc.format(cpu.getNice()));
            println("Combined......" + CpuPerc.format(cpu.getCombined()));
            
            println("");
        }
    }

    public static void main(String[] args) throws Exception {
        new CpuInfo().processCommand(args);
    }
}
