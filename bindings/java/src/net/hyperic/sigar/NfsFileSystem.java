package net.hyperic.sigar;

public class NfsFileSystem extends FileSystem {

    private static native boolean ping(String hostname);

    String hostname = null;

    public String getHostname() {
        if (this.hostname == null) {
            String dev = getDevName();
            int ix = dev.indexOf(":");
            if (ix != -1) {
                this.hostname = dev.substring(0, ix);
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
