package net.hyperic.sigar.ptql;

import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarProxy;

public interface ProcessQuery {

    public boolean match(SigarProxy sigar, long pid) 
        throws SigarException;
}
