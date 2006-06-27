package org.hyperic.sigar.win32.test;

import junit.framework.TestCase;
import org.hyperic.sigar.win32.RegistryKey;

public class TestRegistryKey extends TestCase {

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
