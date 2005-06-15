package net.hyperic.sigar.win32.test;

import net.hyperic.sigar.test.SigarTestCase;
import net.hyperic.sigar.win32.Pdh;

public class TestPdh extends SigarTestCase {

    public TestPdh(String name) {
        super(name);
    }

    private void getValue(String key) throws Exception {
        Pdh pdh = new Pdh();

        assertGtZeroTrace("raw..." + key,
                          (long)pdh.getRawValue(key));
        assertGtZeroTrace("fmt..." + key,
                          (long)pdh.getFormattedValue(key));
    }

    public void testGetValue() throws Exception {
        String[] keys = {
            "\\Memory\\Available Bytes",
            "\\Memory\\Pages/sec",
        };
        for (int i=0; i<keys.length; i++) {
            getValue(keys[i]);
        }
    }

    public void testPdh () throws Exception {

        Pdh pdh = new Pdh();

        String[] iface = Pdh.getKeys("Thread");
        
        assertTrue(iface.length > 0);
    }
}
