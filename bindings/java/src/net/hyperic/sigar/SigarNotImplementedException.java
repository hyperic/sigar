package net.hyperic.sigar;

/**
 * Sigar exception class, thrown for methods which are not implemented
 * on a given platform.
 */
public class SigarNotImplementedException extends SigarException {

    public static final SigarNotImplementedException INSTANCE =
        new SigarNotImplementedException();

    public SigarNotImplementedException () { super(); }

    public SigarNotImplementedException (String s) { super(s); }
}
