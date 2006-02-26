package net.hyperic.sigar;

public class RPC {

    public static final int UDP = NetFlags.CONN_UDP;
    public static final int TCP = NetFlags.CONN_TCP;

    public static final long NFS = 100003;

    public static native boolean ping(String hostname,
                                      int protocol,
                                      long program,
                                      long version);

    public static boolean ping(String hostname, long program) {
        return ping(hostname, UDP, program, 2);
    }
}
