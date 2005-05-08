package net.hyperic.sigar;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

public class NetServices {

    private static final String SERVICE_FILE;
    private static Map udpServices = null;
    private static Map tcpServices = null;
    
    static {
        String defaultFile;

        if (SigarLoader.IS_WIN32) {
            defaultFile = "C:\\windows\\system32\\drivers\\etc\\services";
        }
        else {
            defaultFile = "/etc/services";
        }

        SERVICE_FILE =
            System.getProperty("sigar.net.services.file", defaultFile);
    }
    
    private static void parseServices(String type, Map services) {
        File file = new File(SERVICE_FILE);
        if (!file.exists()) {
            return;
        }
    
        BufferedReader reader = null;
    
        try {
            reader = new BufferedReader(new FileReader(file));
            String line;
            while ((line = reader.readLine()) != null) {
                String name, protocol;
                Long port;
    
                line = line.trim();
                if ((line.length() == 0) || (line.charAt(0) == '#')) {
                    continue;
                }
    
                StringTokenizer st = new StringTokenizer(line, " \t/#");
                if (st.countTokens() < 3) {
                    continue;
                }
                name = st.nextToken().trim();
                String pnum = st.nextToken().trim();
                protocol = st.nextToken().trim();
                if (!type.equals(protocol)) {
                    continue;
                }
                services.put(Long.valueOf(pnum), name);
            }
        } catch (IOException e) {
            return;
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) { }
            }
        }
    }

    public static String getName(String protocol, long port) {
        if (protocol.equals("tcp")) {
            return getTcpName(port);
        }
        else if (protocol.equals("udp")) {
            return getUdpName(port);
        }
        else {
            return String.valueOf(port);
        }
    }

    public static String getTcpName(long port) {
        if (tcpServices == null) {
            tcpServices = new HashMap();
            parseServices("tcp", tcpServices);
        }
        return (String)tcpServices.get(new Long(port));
    }

    public static String getUdpName(long port) {
        if (udpServices == null) {
            udpServices = new HashMap();
            parseServices("udp", udpServices);
        }
        return (String)udpServices.get(new Long(port));
    }
}
