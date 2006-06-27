package org.hyperic.sigar.util;

public class WeakReferenceMap extends ReferenceMap {

    public Object put(Object key, Object value) {
        poll();
        return this.map.put(key,
                            new WeakValue(key, value, this.queue));
    }
}
