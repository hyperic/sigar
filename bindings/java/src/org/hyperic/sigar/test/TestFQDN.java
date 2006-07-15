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
