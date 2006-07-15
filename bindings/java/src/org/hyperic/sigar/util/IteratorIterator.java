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
