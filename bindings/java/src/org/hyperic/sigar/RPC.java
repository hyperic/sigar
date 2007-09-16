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

package org.hyperic.sigar;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

public class RPC {

    private static Map programs = null;

    public static final int UDP = NetFlags.CONN_UDP;
    public static final int TCP = NetFlags.CONN_TCP;

    public static native int ping(String hostname,
                                  int protocol,
                                  long program,
                                  long version);

    public static native String strerror(int status);

    public static int ping(String hostname,
                           int protocol,
                           String program,
                           long version) {
        return ping(hostname,
                    protocol,
                    getProgram(program),
                    version);
    }

    public static int ping(String hostname, long program) {
        return ping(hostname, UDP, program, 2);
    }

    public static int ping(String hostname, String program) {
        return ping(hostname, UDP, program, 2);
    }

    private static void parse(String fileName) {
        programs = new HashMap();
        File file = new File(fileName);
        if (!file.exists()) {
            return;
        }
        BufferedReader reader = null;

        try {
            reader = new BufferedReader(new FileReader(file));
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if ((line.length() == 0) || (line.charAt(0) == '#')) {
                    continue;
                }
                StringTokenizer st = new StringTokenizer(line, " \t");
                if (st.countTokens() < 2) {
                    continue;
                }
                String name = st.nextToken().trim();
                String num = st.nextToken().trim();
                programs.put(name, num);
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

    /**
     * @return RPC program number as defined in /etc/rpc
     */
    public static long getProgram(String program) {
        if (programs == null) {
            parse("/etc/rpc");
        }

        Long num;
        Object obj = programs.get(program);
        if (obj == null) {
            return -1;
        }
        if (obj instanceof Long) {
            num = (Long)obj;
        }
        else {
            num = Long.valueOf((String)obj);
            programs.put(program, num);
        }

        return num.longValue();
    }

    public static void main(String[] args) throws Exception {
        Sigar.load();
        int retval = RPC.ping(args[0], args[1]);
        System.out.println("(" + retval + ") " + RPC.strerror(retval));
    }
}
