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

import java.util.NoSuchElementException;
import java.util.StringTokenizer;

class QueryBranch {
    String attrClass;
    String attr;
    String op;
    
    QueryBranch(String query)
        throws MalformedQueryException {

        StringTokenizer st = new StringTokenizer(query, ".");

        try {
            this.attrClass = st.nextToken();
        } catch (NoSuchElementException e) {
            throw new MalformedQueryException("Empty query");
        }

        try {
            this.attr = st.nextToken();
        } catch (NoSuchElementException e) {
            throw new MalformedQueryException("Missing attribute");
        }
        
        try {
            this.op = st.nextToken();
        } catch (NoSuchElementException e) {
            throw new MalformedQueryException("Missing operator");
        }
    }

    static String[] split(String query)
        throws MalformedQueryException {

        String[] vals = new String[2];

        int ix = query.indexOf('=');

        if (ix < 0) {
            throw new MalformedQueryException("Missing '='");
        }

        vals[0] = query.substring(0, ix);
        vals[1] = query.substring(ix+1, query.length());

        return vals;
    }
}
