package net.hyperic.sigar.test;

public class TestFQDN extends SigarTestCase {

    public TestFQDN(String name) {
        super(name);
    }

    public void testCreate() throws Exception {

        String fqdn = getSigar().getFQDN();

        traceln("fqdn=" + fqdn);

        boolean validFQDN = fqdn.indexOf(".") > 0;
        /*
        if (!validFQDN) {
            //wont get a valid fqdn on laptop at home
            //allow to fake with ant -Dsigar.fqdn=foo.bar
            String pfake = getProperty("sigar.fqdn");
            String fake = 
                System.getProperty("sigar.fqdn", pfake);
            if ("".equals(fake)) {
                fake = pfake;
            }
            if (fake != null) {
                traceln("fake='" + fake + "'");
                validFQDN = fake.indexOf(".") > 0;
            }
        }
        */
        assertTrue(validFQDN);
    }
}
