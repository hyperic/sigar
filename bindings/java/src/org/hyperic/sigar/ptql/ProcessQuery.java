package org.hyperic.sigar.ptql;

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;

public interface ProcessQuery {

    public boolean match(SigarProxy sigar, long pid) 
        throws SigarException;
}
