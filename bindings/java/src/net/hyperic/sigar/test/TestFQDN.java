package net.hyperic.sigar.test;

import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

import net.hyperic.sigar.Sigar;

public class TestFQDN extends SigarTestCase {

    public TestFQDN(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Properties props = new Properties();
        File f = new File(System.getProperty("user.home"),
                          ".sigar.properties");

        if (f.exists()) {
            try {
                props.load(new FileInputStream(f));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        String fqdn = getSigar().getFQDN();

        traceln("fqdn=" + fqdn);

        boolean validFQDN = fqdn.indexOf(".") > 0;
        if (!validFQDN) {
            //wont get a valid fqdn on laptop at home
            //allow to fake with ant -Dsigar.fqdn=foo.bar
            String pfake = props.getProperty("sigar.fqdn");
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
        assertTrue(validFQDN);
    }
}
