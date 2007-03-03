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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Pdh extends Win32 {

    public static final String PERFLIB_KEY =
        "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Perflib";

    private long   query = -1l; // Handle to the query
    private String hostname = null;

    public Pdh() throws Win32Exception {
        this.query = pdhOpenQuery();
    }
    
    public Pdh(String hostName) throws Win32Exception {
        this();
        this.hostname = hostName;
    }

    protected void finalize() throws Throwable {
        try {
            this.close();
        } finally {
            super.finalize();
        }
    }

    public synchronized void close() throws Win32Exception {
        if (this.query != -1l) {
            pdhCloseQuery(this.query);
            this.query = -1l;
        }
    }

    private static class PerflibCounterMap extends ArrayList {
        private Map map = new HashMap();
        private String index = null;

        //called by RegistryKey.getMultiStringValue
        //format description see: http://support.microsoft.com/kb/q287159/
        public boolean add(Object o) {
            if (index == null) {
                index = (String)o;
                return true;
            }
            //name -> index
            this.map.put(o, index);
            index = null; //reset
            return true;
        }
    }

    public static Map getEnglishPerflibCounterMap()
        throws Win32Exception {

        LocaleInfo locale =
            new LocaleInfo(LocaleInfo.LANG_ENGLISH);

        return getPerflibCounterMap(locale);
    }

    public static Map getPerflibCounterMap(LocaleInfo locale)
        throws Win32Exception {

        String path =
            PERFLIB_KEY + "\\" + locale.getPerflibLangId();

        RegistryKey key =
            RegistryKey.LocalMachine.openSubKey(path);

        PerflibCounterMap counters = new PerflibCounterMap();
        try {
            key.getMultiStringValue("Counter", counters);
        } finally {
            key.close();
        }

        return counters.map;
    }

    public static String getCounterName(int index)
        throws Win32Exception {

        return pdhLookupPerfName(index);
    }

    /**
     * @deprecated
     * @see #getRawValue(String path)
     */
    public double getSingleValue(String path) throws Win32Exception {
        return getRawValue(path);
    }

    public double getRawValue(String path) throws Win32Exception {
        return getValue(path, false);
    }

    public double getFormattedValue(String path) throws Win32Exception {
        return getValue(path, true);
    }

    private double getValue(String path, boolean format)
        throws Win32Exception {

        if (this.hostname != null) {
            pdhConnectMachine(this.hostname);
        }

        long counter = pdhAddCounter(this.query, path);
        try {
            return pdhGetValue(this.query, counter, format);
        } finally {
            pdhRemoveCounter(counter);
        }
    }

    public static String[] getInstances(String path) throws Win32Exception {
        return pdhGetInstances(path);
    }

    public static String[] getKeys(String path) throws Win32Exception {
        return pdhGetKeys(path);
    }

    public static String[] getObjects() throws Win32Exception {
        return pdhGetObjects();
    }

    private static final native void pdhConnectMachine(String host)
        throws Win32Exception;
    private static final native long pdhOpenQuery() throws Win32Exception;
    private static final native void pdhCloseQuery(long query)
        throws Win32Exception;
    private static final native long pdhAddCounter(long query, String path)
        throws Win32Exception;
    private static final native void pdhRemoveCounter(long counter)
        throws Win32Exception;
    private static final native double pdhGetValue(long query, 
                                                   long counter,
                                                   boolean fmt)
        throws Win32Exception;
    private static final native String[] pdhGetInstances(String path)
        throws Win32Exception;
    private static final native String[] pdhGetKeys(String path)
        throws Win32Exception;
    private static final native String[] pdhGetObjects()
        throws Win32Exception;
    private static final native String pdhLookupPerfName(int index)
        throws Win32Exception;

    /**
     * Main method for dumping the entire PDH
     *
     * Usage: Pdh [OPTION]
     * Show information from the Windows PDH
     *
     * -v, --values         include key values [default=no]
     *     --object=NAME    only print info on this object
     *     --contains=NAME  only print info on objects that contain 
                            this substring
     * -i, --instance       show instances [default=no]
     * -k, --keys           show keys [default=no]
     * -h, --help           display help and exit
     */
    public static void main(String args[]) {

        Pdh      pdh           = null;
        String   objectName    = null;
        String   partialName   = null;
        boolean  showValues    = false;
        boolean  showInstances = false;
        boolean  showKeys      = false;

        // Parse command line arguments
        if (args.length > 0) {
            for (int i = 0; i < args.length; i++) {
                if (args[i].equals("-h") ||
                    args[i].equals("-help") ||
                    args[i].equals("--help")) {
                    System.out.println("Usage: Pdh [OPTION]");
                    System.out.println("Show information from the Windows " +
                                       "PDH");
                    System.out.println("");
                    System.out.println("    --object=NAME    " +
                                       "only print info on this object");
                    System.out.println("    --contains=NAME  " +
                                       "only print info on objects that");
                    System.out.println("                     " +
                                       "contain this substring");
                    System.out.println("-i, --instance       " +
                                       "show instances [default=no]");
                    System.out.println("-k, --keys           " +
                                       "show keys [default=no]");
                    System.out.println("-v, --values         " +
                                       "include key values [default=no]");
                    System.out.println("-h, --help           " +
                                       "display help and exit");
                    return;
                } else if (args[i].equals("-v") ||
                           args[i].equals("--values")) {
                    showKeys   = true;  // Assume -k when -v is used.
                    showValues = true;
                } else if (args[i].equals("-i") ||
                           args[i].equals("--instances")) {
                    showInstances = true;
                } else if (args[i].equals("-k") ||
                           args[i].equals("--keys")) {
                    showKeys = true;
                } else if (args[i].startsWith("--contains=")) {
                    int idx = args[i].indexOf("=");
                    partialName = args[i].substring(idx + 1);
                } else if (args[i].startsWith("--object=")) {
                    int idx = args[i].indexOf("=");
                    objectName = args[i].substring(idx + 1);
                } else {
                    System.out.println("Unknown option: " + args[i]);
                    System.out.println("Use --help for usage information");
                    return;
                }
            }
        }
        
        try {
            pdh = new Pdh();

            String[] objects;  // The list of objects to inspect.

            if (partialName != null) {
                // Check list of object names for a user defined
                // substring. (e.g. --contains=PLUMTREE for example)
                List matching = new ArrayList();
                String[] allObjects = Pdh.getObjects();

                for (int i = 0; i < allObjects.length; i++) {
                    if (allObjects[i].toUpperCase().
                        indexOf(partialName.toUpperCase()) != -1) {
                        matching.add(allObjects[i]);
                    }
                }
                objects = (String[])matching.toArray(new String[0]);

            } else if (objectName != null) {
                // Query for a single object
                objects = new String[] { objectName };
            } else {
                objects = Pdh.getObjects();
            }

            for (int o = 0; o < objects.length; o++) {
                System.out.println(objects[o]);

                // Get the query keys for this object
                String[] keys;
                try {
                    keys = Pdh.getKeys(objects[o]);
                } catch (Win32Exception e) {
                    System.err.println("Unable to get keys for object=" +
                                       objects[o] + " Reason: " +
                                       e.getMessage());
                    continue;
                }

                int pad = getLongestKey(keys);

                // Get the instances for this object
                String[] instances = Pdh.getInstances(objects[o]);
                if (instances.length == 0) {
                    // No instances, dump the keys and values for the 
                    // top level object

                    if (showKeys == false)
                        continue;

                    for (int k = 0; k < keys.length; k++) {
                        if (showValues) {
                            String query =
                                "\\" + objects[o] + "\\" + keys[k];
                            double val;

                            try {
                                val = pdh.getRawValue(query);
                            } catch (Win32Exception e) {
                                System.err.println("Unable to get value for " +
                                                   " key=" + query +
                                                   " Reason: " + e.getMessage());
                                continue;
                            }

                            String out = pad(keys[k], pad, ' ');
                            System.out.println("  " + out + " = " + val);
                        } else {
                            System.out.println("  " + keys[k]);
                        }
                    }
                } else {
                    // Only show instance level info if asked.
                    if (showInstances == false)
                        continue;

                    // For each instance, print it along with the keys
                    for (int i = 0; i < instances.length; i++) {
                        System.out.println("  " + instances[i]);
                        // Dump the keys for this instance
                        
                        if (showKeys == false)
                            continue;

                        for (int k = 0; k < keys.length; k++) {
                            if (showValues) {
                                String query = 
                                    "\\" + objects[o] + 
                                    "(" + instances[i] + ")" +
                                    "\\" + keys[k];

                                double val;

                                try {
                                    val = pdh.getRawValue(query);
                                } catch (Win32Exception e) {
                                    System.err.println("Unable to get value " +
                                                       "for key=" + query +
                                                       " Reason: " + 
                                                       e.getMessage());

                                    continue;
                                }
                                
                                String out = pad(keys[k], pad, ' ');
                                System.out.println("    " + out + " = " +
                                                   val);
                            } else {
                                System.out.println("    " + keys[k]);
                            }
                        }
                    }
                }
            }

            pdh.close();

        } catch (Win32Exception e) {
            // Should never happen
            System.err.println("Unable to dump PDH data: " +
                               e.getMessage());
            return;
        }
    }
    
    /**
     * String padder yanked from java-util's StringUtil.java
     */
    private static String pad(String value, int length, char ch) {
        StringBuffer padder = new StringBuffer(value);
        if (value.length() < length) {
            for (int i=0; i < (length - value.length()); i++) {
                padder.append(ch);
            }
        }
        return padder.toString();
    }

    /**
     * Returns the length of the longest string in an array
     */
    private static int getLongestKey(String[] keys) {
        int longest = 0;

        for (int i = 0; i < keys.length; i++) {
            int len = keys[i].length();
            if (len > longest)
                longest = len;
        }

        return longest;
    }
}
