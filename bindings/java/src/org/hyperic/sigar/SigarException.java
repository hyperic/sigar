package org.hyperic.sigar;

/**
 * Sigar base exception class.
 */
public class SigarException extends Exception {

    public SigarException () { super(); }

    public SigarException (String s) { super(s); }
}
