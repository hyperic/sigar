package net.hyperic.sigar.win32.test;

import junit.framework.TestCase;
import net.hyperic.sigar.win32.Pdh;

public class TestPdh extends TestCase {

    public TestPdh(String name) {
        super(name);
    }

    public void testPdhSingleValue() throws Exception {

        Pdh pdh = new Pdh();
        String key = "\\Memory\\Available Bytes";
        double val = pdh.getSingleValue(key);

        assertTrue(val > 0);
    }

    public void testPdh () throws Exception {

        Pdh pdh = new Pdh();

        String[] iface = Pdh.getKeys("Thread");
        
        assertTrue(iface.length > 0);
    }
}
