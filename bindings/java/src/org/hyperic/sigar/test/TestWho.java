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

import java.util.Date;

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.Who;

public class TestWho extends SigarTestCase {

    public TestWho(String name) {
        super(name);
    }

    public void testWho() throws SigarException {
        traceln("");
        Who[] who = getSigar().getWhoList();
        for (int i=0; i<who.length; i++) {
            String host = who[i].getHost();
            if (host.length() != 0) {
                host = "(" + host + ")";
            }
            traceln(who[i].getUser() + "\t" +
                    who[i].getDevice() + "\t" +
                    new Date(who[i].getTime() * 1000) + "\t" +
                    host);
            assertLengthTrace("user", who[i].getUser());
        }
    }
}
