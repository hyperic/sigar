package org.hyperic.sigar.ptql;

/**
 * Exception for malformed process queries which cannot
 * be parsed.
 */
public class MalformedQueryException extends Exception {

    public MalformedQueryException() {
    }

    public MalformedQueryException(String message) {
        super(message);
    }
}
