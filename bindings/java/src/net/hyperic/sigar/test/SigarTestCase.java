package net.hyperic.sigar.test;

import java.io.PrintStream;
import junit.framework.TestCase;

//helper to add optional tracing.
public abstract class SigarTestCase extends TestCase {

    private static boolean verbose = false;
    private static PrintStream out = System.out;

    public SigarTestCase(String name) {
        super(name);
    }

    public static void setVerbose(boolean value) {
        verbose = value;
    }

    public static boolean getVerbose() {
        return verbose;
    }

    public static void setWriter(PrintStream value) {
        out = value;
    }

    public static PrintStream getWriter() {
        return out;
    }

    public void traceln(String msg) {
        if (getVerbose()) {
            getWriter().println(msg);
        }
    }

    public void trace(String msg) {
        if (getVerbose()) {
            getWriter().print(msg);
        }
    }

    public void assertTrueTrace(String msg, String value) {
        traceln(msg + "=" + value);
        assertTrue(msg, value != null);
    }

    public void assertLengthTrace(String msg, String value) {
        assertTrueTrace(msg, value);
        assertTrue(msg, value.length() > 0);
    }

    public void assertGtZeroTrace(String msg, long value) {
        traceln(msg + "=" + value);
        assertTrue(msg, value > 0);
    }

    public void assertGtEqZeroTrace(String msg, long value) {
        traceln(msg + "=" + value);
        assertTrue(msg, value >= 0);
    }

    public void assertEqualsTrace(String msg, long expected, long actual) {
        traceln(msg + "=" + actual + "/" + expected);
        assertEquals(msg, expected, actual);
    }
}
