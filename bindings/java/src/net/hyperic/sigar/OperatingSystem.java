package net.hyperic.sigar;

import java.io.IOException;
import java.io.File;
import java.io.FileReader;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

public class OperatingSystem {

    public static final String NAME_LINUX   = "Linux";
    public static final String NAME_SOLARIS = "Solaris";
    public static final String NAME_HPUX    = "HPUX";
    public static final String NAME_AIX     = "AIX";
    public static final String NAME_MACOSX  = "MacOSX";
    public static final String NAME_FREEBSD = "FreeBSD";
    public static final String NAME_WIN32   = "Win32";
    public static final String NAME_NETWARE = "NetWare";
    
    public static String[] UNIX_NAMES = {
        OperatingSystem.NAME_LINUX,
        OperatingSystem.NAME_SOLARIS,
        OperatingSystem.NAME_HPUX,
        OperatingSystem.NAME_AIX,
        OperatingSystem.NAME_MACOSX,
        OperatingSystem.NAME_FREEBSD,
    };
        
    public static String[] WIN32_NAMES = {
        OperatingSystem.NAME_WIN32,
    };

    public static final String[] NAMES;
    
    public static final boolean IS_WIN32 =
        System.getProperty("os.name").indexOf("Windows") != -1;
    
    private static Map supportedPlatforms = new HashMap();
    
    static {
        int len = UNIX_NAMES.length + WIN32_NAMES.length;
        String[] all = new String[len];
        System.arraycopy(UNIX_NAMES, 0, all, 0, UNIX_NAMES.length);
        all[len-1] = NAME_WIN32;
        NAMES = all;
        
        for (int i=0; i<NAMES.length; i++) {
            supportedPlatforms.put(NAMES[i], Boolean.TRUE);
        }
    }

    public static boolean isSupported(String name) {
        return supportedPlatforms.get(name) == Boolean.TRUE;
    }

    public static boolean isWin32(String name) {
        return OperatingSystem.NAME_WIN32.equals(name);
    }
    
    private static final int TYPE_LINUX   = 0;
    private static final int TYPE_SOLARIS = 1;
    private static final int TYPE_HPUX    = 2;
    private static final int TYPE_AIX     = 3;
    private static final int TYPE_MACOSX  = 4;
    private static final int TYPE_FREEBSD = 5;
    private static final int TYPE_WIN32   = 6;
    private static final int TYPE_NETWARE = 7;

    private static final String ETC =
        System.getProperty("sigar.etc.dir", "/etc") + "/";

    private static OperatingSystem instance = null;

    private int type;
    private String name;
    private String version;
    private String arch;
    private String patchLevel;
    private String vendor;
    private String vendorVersion;
    private String vendorName;
    private String vendorCodeName;
    private String dataModel;
    private String cpuEndian;
    
    private OperatingSystem() {
    }

    public static synchronized OperatingSystem getInstance() {
        if (instance == null) {
            OperatingSystem os = new OperatingSystem();
            Properties props = System.getProperties();
            os.name = props.getProperty("os.name");
            os.vendorName = os.name;
            os.version = props.getProperty("os.version");
            os.arch = props.getProperty("os.arch");
            os.patchLevel = props.getProperty("sun.os.patch.level");
            os.dataModel = props.getProperty("sun.arch.data.model");
            os.cpuEndian = props.getProperty("sun.cpu.endian");

            if (os.name.equals(NAME_LINUX)) {
                os.type = TYPE_LINUX;
                os.getLinuxInfo();
            }
            else if (IS_WIN32) {
                os.type = TYPE_WIN32;
                os.vendor = "Microsoft";
                if (os.name.endsWith("XP")) {
                    os.vendorVersion = "XP";
                }
                else if (os.name.indexOf("2003") != -1) {
                    os.vendorVersion = "2003";
                }
                else if (os.name.indexOf("2000") != -1) {
                    os.vendorVersion = "2000";
                }
                else if (os.name.indexOf("NT") != -1) {
                    os.vendorVersion = "NT";
                }
                os.name = "Win32";
                if (os.vendorVersion.equals("XP")) {
                    os.vendorCodeName = "Whistler";
                }
                else if (os.vendorVersion.equals("2003")) {
                    os.vendorCodeName = "Whistler Server";
                }
            }
            else if (os.name.equals("SunOS")) {
                os.type = TYPE_SOLARIS;
                os.vendor = "Sun Microsystems";
                if (os.version.equals("5.6")) {
                    os.vendorVersion = "2.6";
                }
                else {
                    int ix = os.version.indexOf(".");
                    //5.8 == solaris 8, etc.
                    os.vendorVersion = os.version.substring(ix+1);
                }
                os.name = NAME_SOLARIS;
            }
            else if (os.name.equals("HP-UX")) {
                os.type = TYPE_HPUX;
                os.name = NAME_HPUX;
                os.vendor = "Hewlett-Packard";
                if (os.version.indexOf(".11.") != -1) {
                    os.vendorVersion = "11";
                }
            }
            else if (os.name.equals(NAME_AIX)) {
                os.type = TYPE_AIX;
                os.vendor = "IBM";
            }
            else if (os.name.equals("Mac OS X")) {
                os.type = TYPE_MACOSX;
                os.name = NAME_MACOSX;
                os.vendor = "Apple";
                if (os.version.startsWith("10.4")) {
                    os.vendorCodeName = "Tiger";
                }
                else if (os.version.startsWith("10.3")) {
                    os.vendorCodeName = "Panther";
                }
            }
            else if (os.name.equals(NAME_FREEBSD)) {
                os.type = TYPE_FREEBSD;
                os.vendor = NAME_FREEBSD;
            }
            else if (os.name.equals(NAME_NETWARE)) {
                os.type = TYPE_NETWARE;
                os.vendor = "Novell";
                os.vendorVersion = os.version.substring(0, 3);
            }
            else {
                os.vendor = "Unknown";
            }

            if (os.vendorVersion == null) {
                os.vendorVersion = os.version;
            }

            instance = os;
        }

        return instance;
    }

    public String getDescription() {
        switch (this.type) {
          case TYPE_LINUX:
            //"Red Hat 8.0"
            return this.vendor + " " + this.vendorVersion;
          case TYPE_SOLARIS:
            //"Solaris 8"
            return this.name + " " + this.vendorVersion;
          case TYPE_HPUX:
            //"HP-UX 11"
            return this.vendorName + " " + this.vendorVersion;
          case TYPE_MACOSX:
            //"Mac OS X Tiger"
            if (this.vendorCodeName != null) {
                return this.vendorName + " " + this.vendorCodeName;
            }
            else {
                return this.vendorName;
            }
          case TYPE_WIN32:
            //"Microsoft Windows 2003"
            return this.vendor + " Windows " + this.vendorVersion;
          case TYPE_NETWARE:
            //"NetWare 6.0"
            return this.name + " " + this.vendorVersion;
          default:
            //"AIX 5.2", "FreeBSD 5.3"
            return this.name + " " + this.version;
        }
    }

    public String getName() {
        return this.name;
    }

    public String getVersion() {
        return this.version;
    }

    public String getArch() {
        return this.arch;
    }

    public String getPatchLevel() {
        return this.patchLevel;
    }

    public String getVendor() {
        return this.vendor;
    }

    public String getVendorVersion() {
        return this.vendorVersion;
    }

    public String getVendorName() {
        return this.vendorName;
    }

    public String getVendorCodeName() {
        return this.vendorCodeName;
    }

    public String getDataModel() {
        return this.dataModel;
    }

    public String getCpuEndian() {
        return this.cpuEndian;
    }

    private void getLinuxInfo() {
        VendorInfo[] info = {
            new GenericVendor("mandrake-release", "Mandrake"),
            new GenericVendor("SuSE-release", "SuSE"),
            new GenericVendor("gentoo-release", "Gentoo"),
            new GenericVendor("debian_version", "Debian"),
            new GenericVendor("slackware-version", "Slackware"),
            new GenericVendor("fedora-release", "Fedora"),
            new RedHatVendor("redhat-release", "Red Hat"),
            new VMwareVendor("/proc/vmware/version", "VMware"),
        };

        for (int i=0; i<info.length; i++) {
            File file = new File(info[i].getFileName());
            if (!file.exists()) {
                continue;
            }

            FileReader reader = null;
            try {
                int len = (int)file.length();
                char[] data = new char[len];
                reader = new FileReader(file);
                reader.read(data, 0, len);
                info[i].parse(new String(data), this);
            } catch (IOException e) {
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException e) { }
                }
            }
            break;
        }
    }

    private static String parseVersion(String line) {
        StringTokenizer tok = new StringTokenizer(line);
        while (tok.hasMoreTokens()) {
            String s = tok.nextToken();
            if (Character.isDigit(s.charAt(0))) {
                int i;
                for (i=1; i<s.length(); i++) {
                    char c = s.charAt(i);
                    if (!(Character.isDigit(c) || (c == '.'))) {
                        break;
                    }
                }
                return s.substring(0, i);
            }
        }
        return null;
    }

    private interface VendorInfo {
        public String getFileName();

        public void parse(String data, OperatingSystem os);
    }

    private class RedHatVendor extends GenericVendor {
        public RedHatVendor(String file, String name) {
            super(file, name);
        }

        public void parse(String line, OperatingSystem os) {
            super.parse(line, os);

            int ix = line.indexOf("(");
            if (ix != -1) {
                String codeName = line.substring(ix+1);
                ix = codeName.indexOf(")");
                codeName = codeName.substring(0, ix);
                os.vendorCodeName = codeName;
            }

            String token = "Red Hat Enterprise Linux ";

            if (line.startsWith(token)) {
                line = line.substring(token.length());
                ix = line.indexOf(" "); //'AS' or 'ES'
                os.vendorVersion =
                    line.substring(0, ix) + " " + os.vendorVersion;
            }
            else if (line.startsWith("CentOS")) {
                os.vendor = "CentOS";
            }
        }
    }

    private class VMwareVendor extends GenericVendor {
        public VMwareVendor(String file, String name) {
            super(file, name);
        }

        public String getFileName() {
            return this.file;
        }

        public void parse(String line, OperatingSystem os) {
            os.vendor = this.name;
            os.vendorVersion = "ESX 2.x"; //XXX
        }
    }

    private class GenericVendor implements VendorInfo {
        protected String file;
        protected String name;

        public GenericVendor(String file, String name) {
            this.file = file;
            this.name = name;
        }

        public String getFileName() {
            return ETC + this.file;
        }

        public void parse(String line, OperatingSystem os) {
            os.vendor = this.name;
            os.vendorVersion = parseVersion(line);
        }
    }

    public static void main(String[] args) {
        System.out.println("all.............." + Arrays.asList(NAMES));
        OperatingSystem os = OperatingSystem.getInstance();
        System.out.println("description......" + os.getDescription());
        System.out.println("name............." + os.name);
        System.out.println("version.........." + os.version);
        System.out.println("arch............." + os.arch);
        System.out.println("patch level......" + os.patchLevel);
        System.out.println("vendor..........." + os.vendor);
        System.out.println("vendor name......" + os.vendorName);
        System.out.println("vendor version..." + os.vendorVersion);
    }
}
