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

import java.util.ArrayList;

import org.hyperic.sigar.test.SigarTestCase;
import org.hyperic.sigar.win32.RegistryKey;

public class TestRegistryKey extends SigarTestCase {

    private static final boolean TEST_WRITE = false;
    
    public TestRegistryKey(String name) {
        super(name);
    }

    public void testRegistryRead() throws Exception {
        RegistryKey software =
            RegistryKey.LocalMachine.openSubKey("SOFTWARE");
        String[] keys = software.getSubKeyNames();
        assertTrue(keys.length > 0);
        software.close();
    }

    public void testHardwareValues() throws Exception {
        RegistryKey hw =
            RegistryKey.LocalMachine.openSubKey("HARDWARE\\DESCRIPTION\\System");
        try {
            ArrayList values = new ArrayList();
            hw.getMultiStringValue("SystemBiosVersion", values);
            assertGtZeroTrace("SystemBiosVersion.size()", values.size());
            traceln("SystemBiosVersion=" + values);
        } catch (Exception e) {}
        RegistryKey cpu0 = hw.openSubKey("CentralProcessor\\0");
        String cpu = cpu0.getStringValue("ProcessorNameString");
        assertLengthTrace("cpu0", cpu);
        cpu0.close();
        hw.close();
    }

    public void testSoftwareValues() throws Exception {
        RegistryKey ms =
            RegistryKey.LocalMachine.openSubKey("SOFTWARE\\Microsoft");

        RegistryKey msmq = null;
        try {
            msmq = ms.openSubKey("MSMQ\\Parameters");
        } catch (Exception e) { /*not installed - ok*/ }
        if (msmq != null) {
            traceln("MSMQ...");
            if (msmq.getSubKeyNames().length > 0) {
                try {
                    String build = msmq.getStringValue("CurrentBuild");
                    assertLengthTrace("CurrentBuild", build);
                    int id = msmq.getIntValue("SeqID");
                    assertGtZeroTrace("SeqID", id);
                } catch (Exception e) {}
            }
            msmq.close();
        }

        RegistryKey sql = null;
        try {
            sql = ms.openSubKey("Microsoft SQL Server\\MSSQL.1\\Setup");
        } catch (Exception e) { /*not installed - ok*/ }
        if (sql != null) {
            traceln("MsSQL...");
            try {
                String edition = sql.getStringValue("Edition");
                assertLengthTrace("Edition", edition);
            } catch (Exception e) {}
            sql.close();
        }
        ms.close();

        final String TC =
            "SOFTWARE\\Apache Software Foundation\\Procrun 2.0\\Tomcat6\\Parameters\\Java";
        try {
            RegistryKey tc =
                RegistryKey.LocalMachine.openSubKey(TC);
            traceln("Tomcat6...");
            ArrayList values = new ArrayList();
            tc.getMultiStringValue("Options", values);
            assertGtZeroTrace("Options.size()", values.size());
            traceln("Options=" + values);
            tc.close();
        } catch (Exception e) {}
    }

    //dont want to be writing to the registry
    public void testRegistryWrite() throws Exception {
        if (!TEST_WRITE) {
            return;
        }
        RegistryKey key = RegistryKey.LocalMachine.
            createSubKey("SOFTWARE\\Hyperic\\Test", "Hyperic Test");
        
        key.setStringValue("TestString", "Hello World");
        key.setIntValue("Test Int", 100);
            
        String[] astrNames = key.getValueNames();
        String strValue = key.getStringValue(astrNames[0]);
        //assertTrue(strValue.equals("Covalent Test"));

        int iValue = key.getIntValue(astrNames[1]);
        //assertTrue(iValue == 100);
        
        key = RegistryKey.LocalMachine.openSubKey("SOFTWARE\\Hyperic");
        astrNames = key.getSubKeyNames();

        // Clean up
        key.deleteSubKey("Test");
    }
}
