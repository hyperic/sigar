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

package org.hyperic.sigar.win32.test;

import java.util.Map;

import org.hyperic.sigar.test.SigarTestCase;
import org.hyperic.sigar.win32.Pdh;

public class TestPdh extends SigarTestCase {

    public TestPdh(String name) {
        super(name);
    }

    private void getValue(String key) throws Exception {
        Pdh pdh = new Pdh();

        assertGtEqZeroTrace("raw..." + key,
                            (long)pdh.getRawValue(key));
        assertGtEqZeroTrace("fmt..." + key,
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
        /* XXX hangs for a while on my XP box
        String bogusKey = "\\Does Not\\Exist";
        try {
            new Pdh().getRawValue(bogusKey);
            assertTrue(false);
        } catch (Win32Exception e) {
            assertTrue(true);
            traceln(bogusKey + "=" + e.getMessage());
        }
        */
    }

    public void testCounterMap() throws Exception {
        Map counters = Pdh.getEnglishPerflibCounterMap();

        assertGtZeroTrace("counters", counters.size());

        String[] keys = {
            "System", "System Up Time"
        };
        String last = null;
        for (int i=0; i<keys.length; i++) {
            String name = keys[i];
            String index = (String)counters.get(name);
            assertFalse(index.equals(last));
            traceln(name + "=" + index);
            last = index;
            String lookupName =
                Pdh.getCounterName(Integer.parseInt(index));
            traceln(name + "=" + lookupName);
        }
    }

    public void testPdh() throws Exception {

        String[] iface = Pdh.getKeys("Thread");
        
        assertTrue(iface.length > 0);
    }
}
