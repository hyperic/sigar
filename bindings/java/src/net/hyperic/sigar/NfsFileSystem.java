package net.hyperic.sigar;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class NfsFileSystem extends FileSystem {

    private static native boolean ping(String hostname);

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
        return ping(getHostname());
    }

    public static void main(String[] args) throws Exception {
        Sigar.load();
        System.out.println(NfsFileSystem.ping(args[0]));
    }
}
