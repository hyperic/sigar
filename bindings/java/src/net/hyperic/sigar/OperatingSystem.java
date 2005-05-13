package net.hyperic.sigar;

import java.io.IOException;
import java.io.File;
import java.io.FileReader;

import java.util.Properties;
import java.util.StringTokenizer;

public class OperatingSystem {

    private static final String ETC =
        System.getProperty("sigar.etc.dir", "/etc") + "/";

    private static OperatingSystem instance = null;

    private String name;
    private String version;
    private String arch;
    private String patchLevel;
    private String vendor;
    private String vendorVersion;
    private String dataModel;
    private String cpuEndian;
    
    private OperatingSystem() {
    }

    public static synchronized OperatingSystem getInstance() {
        if (instance == null) {
            OperatingSystem os = new OperatingSystem();
            Properties props = System.getProperties();
            os.name = props.getProperty("os.name");
            os.version = props.getProperty("os.version");
            os.arch = props.getProperty("os.arch");
            os.patchLevel = props.getProperty("sun.os.patch.level");
            os.dataModel = props.getProperty("sun.arch.data.model");
            os.cpuEndian = props.getProperty("sun.cpu.endian");
            
            if (os.name.equals("Linux")) {
                os.getLinuxInfo();
            }
            else if (os.name.indexOf("Windows") > -1) {
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
            }
            else if (os.name.equals("SunOS")) {
                os.vendor = "Sun Microsystems";
                int ix = os.version.indexOf(".");
                //5.8 == solaris 8, etc.
                os.vendorVersion = os.version.substring(ix);
                os.name = "Solaris";
            }
            else if (os.name.equals("HP-UX")) {
                os.vendor = "Hewlett-Packard";
                if (os.version.indexOf(".11.") != -1) {
                    os.vendorVersion = "11";
                }
            }
            else if (os.name.equals("OSF1")) {
                //HP acquired DEC
                os.vendor = "Hewlett-Packard";
            }
            else if (os.name.equals("AIX")) {
                os.vendor = "IBM";
            }
            else if (os.name.equals("Mac OS X")) {
                os.vendor = "Apple";
            }
            else if (os.name.equals("FreeBSD")) {
                os.vendor = "FreeBSD";
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

            String token = "Red Hat Enterprise Linux AS";

            if (line.startsWith(token)) {
                os.vendorVersion = "AS " + os.vendorVersion;
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
        OperatingSystem os = OperatingSystem.getInstance();
        System.out.println("name............." + os.name);
        System.out.println("version.........." + os.version);
        System.out.println("arch............." + os.arch);
        System.out.println("patch level......" + os.patchLevel);
        System.out.println("vendor..........." + os.vendor);
        System.out.println("vendor version..." + os.vendorVersion);
    }
}
