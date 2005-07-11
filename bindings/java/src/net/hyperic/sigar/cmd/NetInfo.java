package net.hyperic.sigar.cmd;

import net.hyperic.sigar.SigarException;

/**
 * Display network info.
 */
public class NetInfo extends SigarCommandBase {

    public NetInfo(Shell shell) {
        super(shell);
    }

    public NetInfo() {
        super();
    }

    public String getUsageShort() {
        return "Display network info";
    }

    public void output(String[] args) throws SigarException {
        net.hyperic.sigar.NetInfo info =
            this.sigar.getNetInfo();

        println("domain................" +
                info.getDomain());

        println("default gateway......." +
                info.getDefaultGateway());

        println("primary dns..........." +
                info.getPrimaryDns());

        println("secondary dns........." +
                info.getSecondaryDns());

        println("dhcp server..........." +
                info.getDhcpServer());
    }

    public static void main(String[] args) throws Exception {
        new NetInfo().processCommand(args);
    }
}
