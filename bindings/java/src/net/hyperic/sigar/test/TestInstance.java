package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;

public class TestInstance extends SigarTestCase {

    public TestInstance(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        String fqdn = Sigar.getInstance().getFQDN();
        traceln("\nfqdn=" + fqdn);

        assertTrue(true);

        Sigar sigar = new Sigar();
        sigar.close();

        try {
            sigar.getFQDN();
            //sigar has been closed so we should not get this far.
            assertTrue(false);
        } catch (SigarException e) {
            assertTrue(true);
            traceln(e.getMessage());
        }
    }
}
