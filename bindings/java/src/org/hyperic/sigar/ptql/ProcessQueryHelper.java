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

package org.hyperic.sigar.ptql;

import java.util.Map;
import java.util.HashMap;

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.SigarProxy;

public class ProcessQueryHelper {

    static HashMap stringMatchers = new HashMap();

    static {
        stringMatchers.put("eq", new StringEqMatcher());
        stringMatchers.put("ne", new StringNeMatcher());
        stringMatchers.put("sw", new StringSwMatcher());
        stringMatchers.put("ew", new StringEwMatcher());
        stringMatchers.put("re", new StringReMatcher());
        stringMatchers.put("ct", new StringCtMatcher());
    }

    //avoid NPE if key does not exist.
    public static String getProcEnv(SigarProxy proxy, long pid, String key)
        throws SigarException, SigarNotImplementedException {

        Map vars;

        try {
            vars = proxy.getProcEnv(pid);
        } catch (SigarNotImplementedException e) {
            throw e;
        } catch (SigarException e) {
            return "";
        }

        String val = (String)vars.get(key);

        if (val == null) {
            return "";
        }

        return val;
    }

    //avoid ArrayOutOfBoundsException
    public static String getProcArg(SigarProxy proxy, long pid, int num)
        throws SigarException, SigarNotImplementedException {

        String[] args;

        try {
            args = proxy.getProcArgs(pid);
        } catch (SigarNotImplementedException e) {
            throw e;
        } catch (SigarException e) {
            return "";
        }

        //e.g. find last element of args: Args.-1.eq=weblogic.Server
        if (num < 0) {
            num += args.length;
        }

        if ((num >= args.length) ||
            (num < 0)) {
            return "";
        }

        return args[num];
    }

    public interface StringMatcher {
        public boolean match(String left, String right);
    }

    static class StringEqMatcher implements StringMatcher {
        public boolean match(String left, String right) {
            return left.equals(right);
        }
    }

    static class StringNeMatcher implements StringMatcher {
        public boolean match(String left, String right) {
            return !left.equals(right);
        }
    }

    static class StringEwMatcher implements StringMatcher {
        public boolean match(String left, String right) {
            return left.endsWith(right);
        }
    }

    static class StringSwMatcher implements StringMatcher {
        public boolean match(String left, String right) {
            return left.startsWith(right);
        }
    }

    static class StringCtMatcher implements StringMatcher {
        public boolean match(String left, String right) {
            return left.indexOf(right) != -1;
        }
    }

    static class StringReMatcher implements StringMatcher {
        public boolean match(String left, String right) {
            return StringPattern.matches(left, right);
        }
    }

    public static boolean argsMatch(SigarProxy proxy, long pid,
                                    String value, String op)
        throws SigarException, SigarNotImplementedException {

        String[] args = proxy.getProcArgs(pid);

        StringMatcher matcher = 
            (StringMatcher)stringMatchers.get(op);

        for (int i=0; i<args.length; i++) {
            if (matcher.match(args[i], value)) {
                return true;
            }
        }

        return false;
    }
}
