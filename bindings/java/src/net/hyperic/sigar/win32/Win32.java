package net.hyperic.sigar.win32;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;

abstract class Win32 {

    static {
        try {
            Sigar.load();
        } catch (SigarException e) {
            
        }
    }
}
