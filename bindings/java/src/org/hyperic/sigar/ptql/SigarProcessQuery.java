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

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;

public class SigarProcessQuery implements ProcessQuery {
    int sigarWrapper = 0; //holds the sigar_ptql_query_t *
    long longSigarWrapper = 0; //same, but where sizeof(void*) > sizeof(int)

    native void create(String ptql)
        throws MalformedQueryException;

    native void destroy();

    protected void finalize() {
        destroy();
    }

    public native boolean match(Sigar sigar, long pid) 
        throws SigarException;

    public native long findProcess(Sigar sigar)
        throws SigarException;

    public native long[] find(Sigar sigar)
        throws SigarException;

    static boolean re(String haystack, String needle) {
        if (haystack == null) {
            return false;
        }
        if (needle == null) {
            return false;
        }
        return StringPattern.matches(haystack, needle);
    }
}
