package net.hyperic.sigar;

/**
 * Flag constants for network related ops.
 */
public class NetFlags {

    private NetFlags () { }

    /**
     * value of unknown or non-existent hardware address
     */
    public final static String NULL_HWADDR = "00:00:00:00:00:00";

    /**
     * interface is up
     */
    public final static int IFF_UP = 0x1;

    /**
     * broadcast address valid
     */
    public final static int IFF_BROADCAST = 0x2;

    /**
     * debugging is on
     */
    public final static int IFF_DEBUG = 0x4;

    /**
     * is a loopback net
     */
    public final static int IFF_LOOPBACK = 0x8;

    /**
     * interface has a point-to-point link
     */
    public final static int IFF_POINTOPOINT = 0x10;

    /**
     * avoid use of trailers
     */
    public final static int IFF_NOTRAILERS = 0x20;

    /**
     * interface is running
     */
    public final static int IFF_RUNNING = 0x40;

    /**
     * no ARP protocol
     */
    public final static int IFF_NOARP = 0x80;

    /**
     * receive all packets
     */
    public final static int IFF_PROMISC = 0x100;

    /**
     * receive all multicast packets
     */
    public final static int IFF_ALLMULTI = 0x200;

    /**
     * supports multicast
     */
    public final static int IFF_MULTICAST = 0x800;

    public final static int CONN_CLIENT = 0x01;
    public final static int CONN_SERVER = 0x02;

    public final static int CONN_TCP  = 0x10;
    public final static int CONN_UDP  = 0x20;
    public final static int CONN_RAW  = 0x40;
    public final static int CONN_UNIX = 0x80;

    public final static int CONN_PROTOCOLS = 
        NetFlags.CONN_TCP | NetFlags.CONN_UDP | 
        NetFlags.CONN_RAW | NetFlags.CONN_UNIX;

    /**
     * @param flags network interface flags.
     * @return String representation of network interface flags.
     * @see net.hyperic.sigar.NetInterfaceConfig#getFlags
     */
    public static String getIfFlagsString(long flags) {
        String retval = "";

        if (flags == 0)
            retval += "[NO FLAGS] ";
        if ((flags & IFF_UP) > 0)
            retval += "UP ";
        if ((flags & IFF_BROADCAST) > 0)
            retval += "BROADCAST ";
        if ((flags & IFF_DEBUG) > 0)
            retval += "DEBUG ";
        if ((flags & IFF_LOOPBACK) > 0)
            retval += "LOOPBACK ";
        if ((flags & IFF_POINTOPOINT) > 0)
            retval += "POINTOPOINT ";
        if ((flags & IFF_NOTRAILERS) > 0)
            retval += "NOTRAILERS ";
        if ((flags & IFF_RUNNING) > 0)
            retval += "RUNNING ";
        if ((flags & IFF_NOARP) > 0)
            retval += "NOARP ";
        if ((flags & IFF_PROMISC) > 0)
            retval += "PROMISC ";
        if ((flags & IFF_ALLMULTI) > 0)
            retval += "ALLMULTI ";
        if ((flags & IFF_MULTICAST) > 0)
            retval += "MULTICAST ";

        return retval;
    }
}
