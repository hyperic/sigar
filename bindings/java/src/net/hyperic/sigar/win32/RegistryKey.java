package net.hyperic.sigar.win32;

import java.util.Collection;
import java.util.Vector;

public class RegistryKey extends Win32Bindings
{
    protected static final int HKEY_CLASSES_ROOT     = 0x80000000;
    protected static final int HKEY_CURRENT_USER     = 0x80000001;
    protected static final int HKEY_LOCAL_MACHINE    = 0x80000002;
    protected static final int HKEY_USERS            = 0x80000003;
    protected static final int HKEY_PERFORMANCE_DATA = 0x80000004;
    protected static final int HKEY_CURRENT_CONFIG   = 0x80000005;
    protected static final int HKEY_DYN_DATA         = 0x80000006;

    public static final RegistryKey ClassesRoot 
        = new RegistryKey(RegistryKey.HKEY_CLASSES_ROOT);
    public static final RegistryKey CurrentUser
        = new RegistryKey(RegistryKey.HKEY_CURRENT_USER);
    public static final RegistryKey LocalMachine 
        = new RegistryKey(RegistryKey.HKEY_LOCAL_MACHINE);
    
    protected long  m_hkey;
    private String subkey;

    protected RegistryKey(long hkey)
    {
        this.m_hkey = hkey;
    }
    
    public void close()
    {
        RegCloseKey(this.m_hkey);
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
        if(this.m_hkey != 0)
            this.close();
    }

    protected static final native int    RegCloseKey(long hkey);
    protected static final native long   RegCreateKey(long hkey, 
                                                      String subkey);
    protected static final native int    RegDeleteKey(long hkey, 
                                                      String subkey);
    protected static final native int    RegDeleteValue(long hkey, 
                                                        String valueName);
    protected static final native String RegEnumKey(long hkey, 
                                                    int index);
    protected static final native String RegEnumValueName(long hkey, 
                                                          int index);
    protected static final native int    RegFlushKey(long hkey);
    protected static final native int    RegLoadKey(long hkey, 
                                                    String subkey, 
                                                    String filename);
    protected static final native long   RegOpenKey(long hkey, String subkey);
    protected static final native byte[] RegQueryBufferValue(long hkey, 
                                                             String valueName);
    protected static final native int    RegQueryIntValue(long hkey,
                                                          String valueName);
    protected static final native String RegQueryStringValue(long hkey, 
                                                             String valueName);
    protected static final native int    RegSetIntValue(long hkey, 
                                                        String valueName,
                                                        int value);
    protected static final native int    RegSetStringValue(long hkey, 
                                                           String valueName, 
                                                           String value);
}
