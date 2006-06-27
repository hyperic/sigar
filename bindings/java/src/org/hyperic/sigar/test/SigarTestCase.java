package org.hyperic.sigar.test;

import java.io.PrintStream;
import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;

import java.util.Properties;

import junit.framework.TestCase;

import org.hyperic.sigar.Sigar;

//helper to add optional tracing.
public abstract class SigarTestCase extends TestCase {

    private static Sigar sigar = null;
    private Properties props = new Properties();

    private static boolean verbose =
        "true".equals(System.getProperty("sigar.testVerbose"));

    private static PrintStream out = System.out;

    public SigarTestCase(String name) {
        super(name);

        File f = new File(System.getProperty("user.home"),
                          ".sigar.properties");

        if (f.exists()) {
            FileInputStream is = null;

            try {
                is = new FileInputStream(f);
                this.props.load(is);
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                if (is != null) {
                    try { is.close(); } catch (IOException e) { }
                }
            }
        }
    }

    public Sigar getSigar() {
        if (sigar == null) {
            sigar = new Sigar();
            if (getVerbose()) {
                sigar.enableLogging(true);
            }
        }
        return sigar;
    }

    public static void closeSigar() {
        if (sigar != null) {
            sigar.close();
            sigar = null;
        }
    }

    public Properties getProperties() {
        return this.props;
    }
    
    public String getProperty(String key, String val) {
        return getProperties().getProperty(key, val);
    }

    public String getProperty(String key) {
        return getProperty(key, null);
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

    public long getInvalidPid() {
        return 666666;
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
