package net.hyperic.sigar.ptql;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarException;

public class WindowsServiceQuery implements ProcessQuery {

    private String name;

    public static native long getServicePid(String name);

    public WindowsServiceQuery(String name) {
        this.name = name;
    }

    public boolean match(SigarProxy sigar, long pid) 
        throws SigarException {

        return pid == getServicePid(this.name);
    }

    public static void main(String[] args) throws Exception {
        Sigar sigar = new Sigar(); //load the .dll
        System.out.println(getServicePid(args[0]));
    }
}
