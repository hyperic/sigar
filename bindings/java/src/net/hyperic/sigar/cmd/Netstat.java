package net.hyperic.sigar.cmd;

import java.net.InetAddress;
import java.net.UnknownHostException;

import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.NetConnection;
import net.hyperic.sigar.NetFlags;
import net.hyperic.sigar.NetPortMap;

/**
 * Display network connections.
 */
public class Netstat extends SigarCommandBase {

    private static boolean isNumeric;

    public Netstat(Shell shell) {
        super(shell);
    }

    public Netstat() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display network connections";
    }

    //poor mans getopt.
    public static int getFlags(String[] args, int flags) {
        int proto_flags = 0;
        isNumeric = false;

        for (int i=0; i<args.length; i++) {
            String arg = args[i];
            int j = 0;

            while (j<arg.length()) {
                switch (arg.charAt(j++)) {
                  case '-':
                    continue;
                  case 'l':
                    flags &= ~NetFlags.CONN_CLIENT;
                    flags |= NetFlags.CONN_SERVER;
                    break;
                  case 'a':
                    flags |= NetFlags.CONN_SERVER | NetFlags.CONN_CLIENT;
                    break;
                  case 'n':
                    isNumeric = true;
                    break;
                  case 't':
                    proto_flags |= NetFlags.CONN_TCP;
                    break;
                  case 'u':
                    proto_flags |= NetFlags.CONN_UDP;
                    break;
                  case 'w':
                    proto_flags |= NetFlags.CONN_RAW;
                    break;
                  case 'x':
                    proto_flags |= NetFlags.CONN_UNIX;
                    break;
                  default:
                    System.err.println("unknown option");
                }
            }
        }

        if (proto_flags != 0) {
            flags &= ~NetFlags.CONN_PROTOCOLS;
            flags |= proto_flags;
        }

        return flags;
    }

    private String formatPort(String proto, long port) {
        if (port == 0) {
            return "*";
        }
        if (!isNumeric) {
            String service = NetPortMap.getServiceName(proto, port);
            if (service != null) {
                return service;
            }
        }
        return String.valueOf(port);
    }

    private String formatAddress(String address) {
        if (isNumeric) {
            return address;
        }
        if (address.equals("0.0.0.0")) {
            return "*";
        }

        //advantage of InetAddress' lookup cache.
        try {
            InetAddress addr = InetAddress.getByName(address);
            return addr.getHostName();
        } catch (UnknownHostException e) {
            return address;
        }
    }

    //XXX currently weak sauce.  should end up like netstat command.
    public void output(String[] args) throws SigarException {
        //default
        int flags = NetFlags.CONN_CLIENT | NetFlags.CONN_PROTOCOLS;

        if (args.length > 0) {
            flags = getFlags(args, flags);
        }

        NetConnection[] connections = this.sigar.getNetConnectionList(flags);
        println("Proto\tLocal Address\tForeign Address");

        for (int i=0; i<connections.length; i++) {
            NetConnection conn = connections[i];
            String proto = conn.getTypeString();
            println(proto +
                    "\t" +
                    formatAddress(conn.getLocalAddress()) + ":" +
                    formatPort(proto, conn.getLocalPort()) +
                    "\t" +
                    formatAddress(conn.getRemoteAddress()) + ":" +
                    formatPort(proto, conn.getRemotePort()));
        }
    }

    public static void main(String[] args) throws Exception {
        new Netstat().processCommand(args);
    }
}
