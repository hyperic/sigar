package net.hyperic.sigar.cmd;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.NetConnection;
import net.hyperic.sigar.NetFlags;

/**
 * Display network connections.
 */
public class Netstat extends SigarCommandBase {

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

            println(conn.getTypeString() +
                    "\t" +
                    conn.getLocalAddress() + ":" +
                    conn.getLocalPort() +
                    "\t" +
                    conn.getRemoteAddress() + ":" +
                    conn.getRemotePort());
        }
    }

    public static void main(String[] args) throws Exception {
        new Netstat().processCommand(args);
    }
}
