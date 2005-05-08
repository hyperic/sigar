package net.hyperic.sigar.cmd;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;

import net.hyperic.sigar.NetServices;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.NetConnection;
import net.hyperic.sigar.NetFlags;

/**
 * Display network connections.
 */
public class Netstat extends SigarCommandBase {

    private static final int LADDR_LEN = 20;
    private static final int RADDR_LEN = 35;

    private static final String OUTPUT_FORMAT =
        "%-5s %-" + LADDR_LEN + "s %-" + RADDR_LEN + "s %s";

    private static final String[] HEADER = new String[] {
        "Proto",
        "Local Address",
        "Foreign Address",
        "State"
    };

    private static boolean isNumeric;

    public Netstat(Shell shell) {
        super(shell);
        setOutputFormat(OUTPUT_FORMAT);
    }

    public Netstat() {
        super();
        setOutputFormat(OUTPUT_FORMAT);
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
            String service = NetServices.getName(proto, port);
            if (service != null) {
                return service;
            }
        }
        return String.valueOf(port);
    }

    private String formatAddress(String proto, String ip,
                                 long portnum, int max) {
        
        String port = formatPort(proto, portnum);
        String address;

        if (isNumeric) {
            address = ip;
        }
        else if (ip.equals("0.0.0.0")) {
            address = "*";
        }
        else {
            try {
                address = InetAddress.getByName(ip).getHostName();
            } catch (UnknownHostException e) {
                address = ip;
            }
        }

        max -= port.length() + 1;
        if (address.length() > max) {
            address = address.substring(0, max);
        }

        return address + ":" + port; 
    }

    //XXX currently weak sauce.  should end up like netstat command.
    public void output(String[] args) throws SigarException {
        //default
        int flags = NetFlags.CONN_CLIENT | NetFlags.CONN_PROTOCOLS;

        if (args.length > 0) {
            flags = getFlags(args, flags);
        }

        NetConnection[] connections = this.sigar.getNetConnectionList(flags);
        printf(HEADER);

        for (int i=0; i<connections.length; i++) {
            NetConnection conn = connections[i];
            String proto = conn.getTypeString();
            String state;

            if (conn.getType() == NetFlags.CONN_UDP) {
                state = "";
            }
            else {
                state = conn.getStateString();
            }

            ArrayList items = new ArrayList();
            items.add(proto);
            items.add(formatAddress(proto,
                                    conn.getLocalAddress(),
                                    conn.getLocalPort(),
                                    LADDR_LEN));
            items.add(formatAddress(proto,
                                    conn.getRemoteAddress(),
                                    conn.getRemotePort(),
                                    RADDR_LEN));
            items.add(state);
            printf(items);
        }
    }

    public static void main(String[] args) throws Exception {
        new Netstat().processCommand(args);
    }
}
