package net.hyperic.sigar;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
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

    interface EntryReader {
        public void process(String name, String port, List aliases);
    }

    static void parse(String fileName, EntryReader entry) {
        File file = new File(fileName);
        if (!file.exists()) {
            return;
        }
        BufferedReader reader = null;
        ArrayList aliases = new ArrayList();

        try {
            reader = new BufferedReader(new FileReader(file));
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if ((line.length() == 0) || (line.charAt(0) == '#')) {
                    continue;
                }
                aliases.clear();
                int ix = line.indexOf("#");
                if (ix != -1) {
                    line = line.substring(0, ix);
                }
                StringTokenizer st = new StringTokenizer(line, " \t");
                if (st.countTokens() < 2) {
                    continue;
                }
                String name = st.nextToken().trim();
                String port = st.nextToken().trim();
                while (st.hasMoreTokens()) {
                    aliases.add(st.nextToken().trim());
                }
                entry.process(name, port, aliases);
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

    private static class ServicesReader implements EntryReader {
        private String protocol;
        private Map services;

        private ServicesReader(String protocol, Map services) {
            this.protocol = protocol;
            this.services = services;
        }

        public void process(String name, String port, List aliases) {
            String pnum, protocol;
            int ix = port.indexOf('/');
            if (ix == -1) {
                return;
            }
            pnum = port.substring(0, ix);
            protocol = port.substring(ix+1);
            if (this.protocol.equals(protocol)) {
                this.services.put(Long.valueOf(pnum), name);
            }
        }
    }

    private static void parseServices(String type, Map services) {
        parse(SERVICE_FILE, new ServicesReader(type, services));
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
