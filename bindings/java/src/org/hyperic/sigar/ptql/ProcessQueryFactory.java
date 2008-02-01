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

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;

public class ProcessQueryFactory {
    
    private static ProcessQueryFactory instance = null;

    private Map cache = new HashMap();

    public ProcessQueryFactory() {}

    public void clear() {
        for (Iterator it=this.cache.values().iterator();
             it.hasNext();)
        {
            SigarProcessQuery query = (SigarProcessQuery)it.next();
            query.destroy();
        }
        this.cache.clear();
    }

    public static ProcessQueryFactory getInstance() {
        if (instance == null) {
            instance = new ProcessQueryFactory();
        }
        return instance;
    }

    public ProcessQuery getQuery(String query)
        throws MalformedQueryException {

        if (query == null) {
            throw new MalformedQueryException("null query");
        }

        if (query.length() == 0) {
            throw new MalformedQueryException("empty query");
        }

        ProcessQuery pQuery = (ProcessQuery)this.cache.get(query);

        if (pQuery != null) {
            return pQuery;
        }

        pQuery = new SigarProcessQuery();
        ((SigarProcessQuery)pQuery).create(query);
        this.cache.put(query, pQuery);
        return pQuery;
    }

    /**
     * @deprecated
     * @see #getQuery(String)
     */
    public static ProcessQuery getInstance(String query)
        throws MalformedQueryException {

        return getInstance().getQuery(query);
    }
}
