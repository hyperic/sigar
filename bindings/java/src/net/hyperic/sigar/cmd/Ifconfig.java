package net.hyperic.sigar.cmd;

import java.util.Arrays;
import java.util.Collection;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.NetInterfaceConfig;
import net.hyperic.sigar.NetInterfaceStat;
import net.hyperic.sigar.NetFlags;

public class Ifconfig extends SigarCommandBase {

    public Ifconfig(Shell shell) {
        super(shell);
    }

    public Ifconfig() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return args.length <= 1;
    }

    public String getSyntaxArgs() {
        return "[interface]";
    }

    public String getUsageShort() {
        return "Network interface information";
    }

    public Collection getCompletions() {
        String[] ifNames;

        try {
            ifNames = this.proxy.getNetInterfaceList();
        } catch (SigarException e) {
            return super.getCompletions();
        }

        return Arrays.asList(ifNames);
    }

    public void output(String[] args) throws SigarException {
        String[] ifNames;

        if (args.length == 1) {
            ifNames = args;
        }
        else {
            ifNames = this.proxy.getNetInterfaceList();
        }

        for (int i=0; i<ifNames.length; i++) {
            output(ifNames[i]);
        }
    }

    public void output(String name) throws SigarException {
        NetInterfaceConfig ifconfig =
            this.sigar.getNetInterfaceConfig(name);
        long flags = ifconfig.getFlags();

        //XXX cannot assume ethernet
        String encap = (flags & NetFlags.IFF_LOOPBACK) > 0 ?
            "Local Loopback" : "Ethernet";

        String hwaddr = "";
        if (!NetFlags.NULL_HWADDR.equals(ifconfig.getHwaddr())) {
            hwaddr = " HWaddr " + ifconfig.getHwaddr();
        }

        println(ifconfig.getName() + "\t" +
                "Link encap:" + encap +
                hwaddr);

        String ptp = "";
        if ((flags & NetFlags.IFF_POINTOPOINT) > 0) {
            ptp = "  P-t-P:" + ifconfig.getDestination();
        }

        String bcast = "";
        if ((flags & NetFlags.IFF_BROADCAST) > 0) {
            bcast = "  Bcast:" + ifconfig.getBroadcast();
        }

        println("\t" +
                "inet addr:" + ifconfig.getAddress() + 
                ptp + //unlikely
                bcast +
                "  Mask:" + ifconfig.getNetmask());

        println("\t" +
                NetFlags.getIfFlagsString(flags) +
                " MTU:" + ifconfig.getMtu() +
                "  Metric:" + ifconfig.getMetric());
        try {
            NetInterfaceStat ifstat =
                this.sigar.getNetInterfaceStat(name);

            println("\t" +
                    "RX packets:" + ifstat.getRxPackets() +
                    " errors:" + ifstat.getRxErrors() +
                    " dropped:" + ifstat.getRxDropped() +
                    " overruns:" + ifstat.getRxOverruns() +
                    " frame:" + ifstat.getRxFrame());

            println("\t" +
                    "TX packets:" + ifstat.getTxPackets() +
                    " errors:" + ifstat.getTxErrors() +
                    " dropped:" + ifstat.getTxDropped() +
                    " overruns:" + ifstat.getTxOverruns() +
                    " carrier:" + ifstat.getTxCarrier());
            println("\t" + "collisions:" +
                    ifstat.getTxCollisions());

            long rxBytes = ifstat.getRxBytes();
            long txBytes = ifstat.getTxBytes();

            println("\t" +
                    "RX bytes:" + rxBytes +
                    " (" + Sigar.formatSize(rxBytes) + ")" +
                    "  " +
                    "TX bytes:" + txBytes + 
                    " (" + Sigar.formatSize(txBytes) + ")");
        } catch (SigarException e) {
        }

        println("");
    }

    public static void main(String[] args) throws Exception {
        new Ifconfig().processCommand(args);
    }
}
