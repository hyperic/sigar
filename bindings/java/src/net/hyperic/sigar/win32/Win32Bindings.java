package net.hyperic.sigar.win32;

import net.hyperic.sigar.Sigar;

abstract class Win32Bindings {

    static {
        new Sigar(); //XXX Sigar.load()
    }
}
