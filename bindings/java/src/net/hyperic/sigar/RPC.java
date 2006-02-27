package net.hyperic.sigar;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RPC {

    private static Map programs = null;

    public static final int UDP = NetFlags.CONN_UDP;
    public static final int TCP = NetFlags.CONN_TCP;

    public static native boolean ping(String hostname,
                                      int protocol,
                                      long program,
                                      long version);

    public static boolean ping(String hostname,
                               int protocol,
                               String program,
                               long version) {
        return ping(hostname,
                    protocol,
                    getProgram(program),
                    version);
    }

    public static boolean ping(String hostname, long program) {
        return ping(hostname, UDP, program, 2);
    }

    public static boolean ping(String hostname, String program) {
        return ping(hostname, UDP, program, 2);
    }

    private static void parse(String fileName) {
        programs = new HashMap();
        NetServices.parse("/etc/rpc", new NetServices.EntryReader() {
            public void process(String program, String num, List aliases) {
                programs.put(program, num);
            }
        });
    }

    /**
     * @return RPC program number as defined in /etc/rpc
     */
    public static long getProgram(String program) {
        if (programs == null) {
            parse("/etc/rpc");
        }

        Long num;
        Object obj = programs.get(program);
        if (obj == null) {
            return -1;
        }
        if (obj instanceof Long) {
            num = (Long)obj;
        }
        else {
            num = Long.valueOf((String)obj);
            programs.put(program, num);
        }

        return num.longValue();
    }
}
