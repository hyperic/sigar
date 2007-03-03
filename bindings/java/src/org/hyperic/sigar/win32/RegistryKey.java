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

package org.hyperic.sigar.win32;

import java.util.Collection;
import java.util.List;
import java.util.Vector;

public class RegistryKey extends Win32
{
    private static final int HKEY_CLASSES_ROOT     = 0x80000000;
    private static final int HKEY_CURRENT_USER     = 0x80000001;
    private static final int HKEY_LOCAL_MACHINE    = 0x80000002;
    private static final int HKEY_USERS            = 0x80000003;
    private static final int HKEY_PERFORMANCE_DATA = 0x80000004;
    private static final int HKEY_CURRENT_CONFIG   = 0x80000005;
    private static final int HKEY_DYN_DATA         = 0x80000006;

    /**
     * HKEY_CLASSES_ROOT
     */
    public static final RegistryKey ClassesRoot 
        = new RegistryKey(RegistryKey.HKEY_CLASSES_ROOT);

    /**
     * HKEY_CURRENT_USER
     */
    public static final RegistryKey CurrentUser
        = new RegistryKey(RegistryKey.HKEY_CURRENT_USER);

    /**
     * HKEY_LOCAL_MACHINE
     */
    public static final RegistryKey LocalMachine 
        = new RegistryKey(RegistryKey.HKEY_LOCAL_MACHINE);
    
    private long m_hkey;
    private String subkey;

    private RegistryKey() { }
    
    private RegistryKey(long hkey)
    {
        this.m_hkey = hkey;
    }
    
    public synchronized void close()
    {
        if (this.m_hkey != 0) {
            RegCloseKey(this.m_hkey);
            this.m_hkey = 0;
        }
    }
    
    public RegistryKey createSubKey(String subkey)
    {
        return new RegistryKey(RegCreateKey(this.m_hkey, subkey));
    }
    
    public String getSubKeyName() {
        return this.subkey;
    }

    public RegistryKey createSubKey(String subkey, String value) 
        throws Win32Exception
    {
        RegistryKey    keyResult = null;
        long           hkey      = RegCreateKey(this.m_hkey, subkey);
        
        if(hkey != 0)
        {
            keyResult = new RegistryKey(hkey);
        
            if(keyResult != null)
                keyResult.setStringValue(null, value);
        }
        else 
            //W32Service.throwLastErrorException();
            throw new Win32Exception("Error creating subkey");

        return keyResult;
    }
    
    public RegistryKey createSubKey(String subkey, int value)
        throws Win32Exception
    {
        RegistryKey keyResult = null;
        long        hkey      = RegCreateKey(this.m_hkey, subkey);
        
        if(hkey != 0)
        {
            keyResult = new RegistryKey(hkey);
        
            if(keyResult != null)
                keyResult.setIntValue(null, value);
        }

        else
            //W32Service.throwLastErrorException();
            throw new Win32Exception("Error creating subkey");

        return keyResult;
    }
    
    public void deleteSubKey(String subkey)
    {
        RegDeleteKey(this.m_hkey, subkey);
    }

    public void deleteSubKeyTree(String subkey)
    {
    }
    
    public void deleteValue(String name)
    {
        RegDeleteValue(this.m_hkey, name);
    }
    
    public void flush()
    {
        RegFlushKey(this.m_hkey);
    }

    public int getIntValue(String name)
        throws Win32Exception
    {
        int iResult = 0;
        
        try {
            iResult = RegQueryIntValue(this.m_hkey, name);
        } catch(Throwable t) {
            //W32Service.throwLastErrorException();
            throw new Win32Exception("Error getting int value");
        }
            
        return iResult;
    }
    
    public int getIntValue(String name, int defaultValue)
    {
        int iResult;
        
        try {
            iResult = this.getIntValue(name);
        } catch(Win32Exception e) {
            iResult = defaultValue;
        }
        
        return iResult;
    }
 
    public String getStringValue(String name) throws Win32Exception
    {
        String strResult = RegQueryStringValue(this.m_hkey, name);
        
        if(strResult == null)
            // W32Service.throwLastErrorException();
            throw new Win32Exception("Error getting string value");

        return strResult;
    }

    public void getMultiStringValue(String name, List values)
        throws Win32Exception
    {
        RegQueryMultiStringValue(this.m_hkey, name, values);
    }
    
    public String getStringValue(String name, String defaultValue)
    {
        String  strResult;
        
        try {
            strResult = this.getStringValue(name);
        } catch(Win32Exception e) {
            strResult = defaultValue;
        }
        
        return strResult;
    }
    
    public String[] getSubKeyNames()
    {
        Collection coll = new Vector();
        String     strName;
        
        for(int i = 0; (strName = RegEnumKey(this.m_hkey, i)) != null; i++)
            coll.add(strName);
        
        return (String[])coll.toArray(new String[coll.size()]);
    }
    
    public String[] getValueNames()
    {
        Collection coll = new Vector();
        String     strName;
        
        for(int i = 0; (strName = RegEnumValueName(this.m_hkey, i)) != null;
            i ++)
            coll.add(strName);
        
        return (String[])coll.toArray(new String[coll.size()]);
    }
    
    public RegistryKey openSubKey(String subkey) throws Win32Exception
    {
        long hkey = RegOpenKey(this.m_hkey, subkey);
        
        if(hkey == 0)
            //W32Service.throwLastErrorException();
            throw new Win32Exception("Error opening subkey");

        RegistryKey key = new RegistryKey(hkey);
        key.subkey = subkey;
        return key;
    }
    
    public void setIntValue(String name, int value) throws Win32Exception
    {
        int iResult = RegSetIntValue(this.m_hkey, name, value);
        
        if(iResult != 0)
            //W32Service.throwLastErrorException();
            throw new Win32Exception("Error setting int value");
    }
    
    public void setStringValue(String name, String value) throws Win32Exception
    {
        int iResult = RegSetStringValue(this.m_hkey, name, value);
        
        if(iResult != 0)
            //W32Service.throwLastErrorException();
            throw new Win32Exception("Error setting string value");
    }
    
    protected void finalize()
    {
        close();
    }

    private static native int    RegCloseKey(long hkey);

    private static native long   RegCreateKey(long hkey, 
                                              String subkey);

    private static native int    RegDeleteKey(long hkey, 
                                              String subkey);

    private static native int    RegDeleteValue(long hkey, 
                                                String valueName);

    private static native String RegEnumKey(long hkey, 
                                            int index);

    private static native String RegEnumValueName(long hkey, 
                                                  int index);

    private static native int    RegFlushKey(long hkey);

    private static native int    RegLoadKey(long hkey, 
                                            String subkey, 
                                            String filename);

    private static native long   RegOpenKey(long hkey, String subkey);

    private static native byte[] RegQueryBufferValue(long hkey, 
                                                     String valueName);

    private static native int    RegQueryIntValue(long hkey,
                                                  String valueName);

    private static native String RegQueryStringValue(long hkey, 
                                                     String valueName);

    private static native void RegQueryMultiStringValue(long hkey, 
                                                        String valueName,
                                                        List values);

    private static native int    RegSetIntValue(long hkey, 
                                                String valueName,
                                                int value);

    private static native int    RegSetStringValue(long hkey, 
                                                   String valueName, 
                                                   String value);
}
