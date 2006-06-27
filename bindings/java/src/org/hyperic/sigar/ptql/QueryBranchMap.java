package org.hyperic.sigar.ptql;

import java.util.ArrayList;
import java.util.HashMap;

public class QueryBranchMap extends HashMap {

    ArrayList branches = new ArrayList();

    public Object put(Object key, Object value) {
        branches.add(key);
        return super.put(key, value);
    }
}
