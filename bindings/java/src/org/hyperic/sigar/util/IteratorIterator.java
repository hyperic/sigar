package org.hyperic.sigar.util;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * Iterator for multiple Iterators.
 */
public class IteratorIterator implements Iterator {

    private int ix = 0;
    private Iterator curr = null;

    private ArrayList iterators = new ArrayList();

    public IteratorIterator() { }

    public void add(Iterator iterator) {
        this.iterators.add(iterator);
    }

    public boolean hasNext() {
        int size = this.iterators.size();

        //first time through
        if (this.curr == null) {
            if (size == 0) {
                return false;
            }

            this.curr = (Iterator)this.iterators.get(0);
        }

        if (this.curr.hasNext()) {
            return true;
        }

        this.ix++;
        if (this.ix >= size) {
            return false;
        }

        this.curr = (Iterator)this.iterators.get(this.ix);

        //recurse in the event that this.curr is empty
        return hasNext();
    }

    public Object next() {
        return this.curr.next();
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}
