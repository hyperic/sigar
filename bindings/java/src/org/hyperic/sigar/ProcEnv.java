package org.hyperic.sigar;

import java.util.Map;

/**
 * Lookup environment for a process.
 */
class ProcEnv {

    private ProcEnv () { }

    /**
     * @param sigar The Sigar object.
     * @param pid Process id.
     * @return Map of environment.
     * @exception SigarException on failure.
     * @see org.hyperic.sigar.Sigar#getProcEnv
     */
    public static native Map getAll(Sigar sigar, long pid)
        throws SigarException;

    /**
     * @param sigar The Sigar object.
     * @param pid Process id.
     * @param key Environment variable name.
     * @return Environment variable value.
     * @exception SigarException on failure.
     * @see org.hyperic.sigar.Sigar#getProcEnv
     */
    public static native String getValue(Sigar sigar, long pid, String key)
        throws SigarException;
}
