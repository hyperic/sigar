/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

package org.hyperic.sigar.test;

import java.io.PrintStream;
import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.lang.reflect.Method;
import java.util.Properties;

import junit.framework.TestCase;

import org.hyperic.sigar.Sigar;

//helper to add optional tracing.
public abstract class SigarTestCase extends TestCase {

    private static Sigar sigar = null;
    private Properties props = new Properties();

    private static boolean verbose =
        "true".equals(System.getProperty("sigar.testVerbose"));

    protected static final boolean JDK_14_COMPAT =
        System.getProperty("java.specification.version").compareTo("1.4") >= 0;

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

    public void assertIndexOfTrace(String msg, String value,
                                   String substr) {
        assertTrueTrace(msg, value);
        assertTrue(msg, value.indexOf(substr) != -1);
    }

    public void assertGtZeroTrace(String msg, long value) {
        traceln(msg + "=" + value);
        assertTrue(msg, value > 0);
    }

    public void assertGtEqZeroTrace(String msg, long value) {
        traceln(msg + "=" + value);
        assertTrue(msg, value >= 0);
    }

    public void assertValidFieldTrace(String msg, long value) {
        if (value != Sigar.FIELD_NOTIMPL) {
            assertGtEqZeroTrace(msg, value);
        }
    }

    public void assertEqualsTrace(String msg, long expected, long actual) {
        traceln(msg + "=" + actual + "/" + expected);
        assertEquals(msg, expected, actual);
    }

    public void traceMethods(Object obj) throws Exception {
        Class cls = obj.getClass();
        Method[] methods = cls.getDeclaredMethods();
        traceln("");
        for (int i=0; i<methods.length; i++) {
            String name = methods[i].getName();
            if (!name.startsWith("get")) {
                continue;
            }
            Object val = methods[i].invoke(obj, new Object[0]);
            if ((val instanceof Long) &&
                ((Long)val).longValue() == Sigar.FIELD_NOTIMPL)
            {
                val = "NOTIMPL";
            }
            traceln(name + "=" + val);
        }
    }
}
