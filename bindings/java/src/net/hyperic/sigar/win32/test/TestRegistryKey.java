package net.hyperic.sigar.win32.test;

import junit.framework.TestCase;
import net.hyperic.sigar.win32.RegistryKey;

public class TestRegistryKey extends TestCase {

    public TestRegistryKey(String name) {
        super(name);
    }

    public void testRegistryKey() throws Exception {

        RegistryKey key = RegistryKey.LocalMachine.
            createSubKey("SOFTWARE\\Covalent\\Test", "Covalent Test");

        key.setStringValue("TestString", "Hello World");
        key.setIntValue("Test Int", 100);
            
        String[] astrNames = key.getValueNames();
        String strValue = key.getStringValue(astrNames[0]);
        //assertTrue(strValue.equals("Covalent Test"));

        int iValue = key.getIntValue(astrNames[1]);
        //assertTrue(iValue == 100);
        
        key = RegistryKey.LocalMachine.openSubKey("SOFTWARE\\Covalent");
        astrNames = key.getSubKeyNames();

        // Clean up
        key.deleteSubKey("Test");
    }
}
