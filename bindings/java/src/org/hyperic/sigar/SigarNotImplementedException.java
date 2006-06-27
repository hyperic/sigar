package org.hyperic.sigar;

/**
 * Sigar exception class, thrown for methods which are not implemented
 * on a given platform.
 */
public class SigarNotImplementedException extends SigarException {

    private static final String msg = 
        "This method has not been implemented on this platform";

    public static final SigarNotImplementedException INSTANCE =
        new SigarNotImplementedException(msg);

    public SigarNotImplementedException () { super(); }

    public SigarNotImplementedException (String s) { super(s); }
}
