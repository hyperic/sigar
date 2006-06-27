package org.hyperic.sigar.ptql;

/**
 * Exception for process queries which were
 * parsed ok but whos class generation failed.
 */
public class QueryLoadException extends Exception {

    public QueryLoadException() {
    }

    public QueryLoadException(String message) {
        super(message);
    }
}
