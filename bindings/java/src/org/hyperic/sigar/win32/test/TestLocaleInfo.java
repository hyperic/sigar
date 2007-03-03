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

import org.hyperic.sigar.test.SigarTestCase;
import org.hyperic.sigar.win32.LocaleInfo;

public class TestLocaleInfo extends SigarTestCase {
    
    public TestLocaleInfo(String name) {
        super(name);
    }

    private void checkInfo(LocaleInfo info, String match)
        throws Exception {

        assertGtZeroTrace("id", info.getId());
        assertGtZeroTrace("primary lang", info.getPrimaryLangId());
        assertGtEqZeroTrace("sub lang", info.getSubLangId());
        assertLengthTrace("perflib id", info.getPerflibLangId());
        assertIndexOfTrace("lang",
                           info.toString(), match);
    }

    public void testInfo() throws Exception {
        Object[][] tests = {
            { new Integer(0x16), "Portuguese" },
            { new Integer(LocaleInfo.makeLangId(0x09,0x05)), "New Zealand" },
            { new Integer(0x07), "German" },
            { new Integer(LocaleInfo.makeLangId(0x0a,0x14)), "Puerto Rico" },
        };

        for (int i=0; i<tests.length; i++) {
            Integer id = (Integer)tests[i][0];
            String lang = (String)tests[i][1];
            LocaleInfo info = new LocaleInfo(id);
            checkInfo(info, lang);
        }

        checkInfo(new LocaleInfo(), "");
    }
}
