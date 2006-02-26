package net.hyperic.sigar;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class NfsFileSystem extends FileSystem {

    String hostname = null;

    public String getHostname() {
        if (this.hostname == null) {
            String dev = getDevName();
            int ix = dev.indexOf(":");
            if (ix != -1) {
                String host = dev.substring(0, ix);
                InetAddress addr;
                //try converting to ip in java land to take
                //advantage of InetAddress' lookup cache.
                try {
                    addr = InetAddress.getByName(host);
                    this.hostname = addr.getHostAddress();
                } catch (UnknownHostException e) {
                    this.hostname = host;
                }
            }
        }
        return this.hostname;
    }

    public boolean ping() {
        return RPC.ping(getHostname(), RPC.NFS);
    }

    public String getUnreachableMessage() {
        return getDevName() + " nfs server unreachable";
    }

    public NfsUnreachableException getUnreachableException() {
        return new NfsUnreachableException(getUnreachableMessage());
    }

    public static void main(String[] args) throws Exception {
        Sigar.load();
        System.out.println(RPC.ping(args[0], RPC.NFS));
    }
}
