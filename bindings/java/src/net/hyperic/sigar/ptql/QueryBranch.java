package net.hyperic.sigar.ptql;

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
