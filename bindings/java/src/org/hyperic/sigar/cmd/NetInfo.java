package org.hyperic.sigar.cmd;

import org.hyperic.sigar.SigarException;

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
        org.hyperic.sigar.NetInfo info =
            this.sigar.getNetInfo();

        println("host name............." +
                info.getHostName());

        println("domain name..........." +
                info.getDomainName());

        println("default gateway......." +
                info.getDefaultGateway());

        println("primary dns..........." +
                info.getPrimaryDns());

        println("secondary dns........." +
                info.getSecondaryDns());
    }

    public static void main(String[] args) throws Exception {
        new NetInfo().processCommand(args);
    }
}
