package org.hyperic.sigar.ptql;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarException;

public class WindowsServiceQuery implements ProcessQuery {

    private String name;

    public WindowsServiceQuery(String name) {
        this.name = name;
    }

    public boolean match(SigarProxy sigar, long pid) 
        throws SigarException {

        return pid == sigar.getServicePid(this.name);
    }

    public static void main(String[] args) throws Exception {
        Sigar sigar = new Sigar(); //load the .dll
        System.out.println(sigar.getServicePid(args[0]));
    }
}
