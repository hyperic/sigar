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

package org.hyperic.sigar.pager;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;

/**
 * A PageFetcher which works with a pre-fetched list as the 
 * data backing the fetcher.
 */

public class ListPageFetcher extends PageFetcher {
    private List data;
    private int  sortOrder;

    public ListPageFetcher(List data) {
        super();
        this.data      = data;
        this.sortOrder = PageControl.SORT_UNSORTED;
    }

    public PageList getPage(PageControl control) {
        PageList res = new PageList();
        int startIdx, curIdx, endIdx;

        if (this.data.size() == 0) {
            return new PageList();
        }

        this.ensureSortOrder(control);
        res.setTotalSize(this.data.size());

        startIdx = clamp(control.getPageEntityIndex(), 0, 
                         this.data.size() - 1);
        curIdx   = startIdx;

        if (control.getPagesize() == PageControl.SIZE_UNLIMITED) {
            endIdx = this.data.size();
        }
        else {
            endIdx = clamp(startIdx + control.getPagesize(), startIdx,
                           this.data.size());
        }

        for (ListIterator i=this.data.listIterator(startIdx); 
            i.hasNext() && curIdx < endIdx; 
            curIdx++)
        {
            res.add(i.next());
        }

        return res;
    }

    private class DescSorter implements Comparator {
        public int compare(Object o1, Object o2){
            return -((Comparable)o1).compareTo((Comparable)o2);
        }

        public boolean equals(Object other){
            return false;
        }
    }

    private void ensureSortOrder(PageControl control) {
        if (control.getSortorder() == this.sortOrder) {
            return;
        }
        
        this.sortOrder = control.getSortorder();
        if (this.sortOrder == PageControl.SORT_UNSORTED) {
            return;
        }
        else if(this.sortOrder == PageControl.SORT_ASC) {
            Collections.sort(data);
        }
        else if(this.sortOrder == PageControl.SORT_DESC) {
            Collections.sort(data, new DescSorter());
        }
        else {
            throw new IllegalStateException("Unknown control sorting type: " +
                                            this.sortOrder);
        }
    }

    /**
     * Clamp a value to a range.  If the passed value is 
     * less than the minimum, return the minimum.  If it
     * is greater than the maximum, assign the maximum.
     * else return the passed value.
     */
    private static int clamp(int val, int min, int max) {
        return (int)clamp((long)val, (long)min, (long)max);
    }

    private static long clamp(long val, long min, long max) {
        if (val < min) {
            return min;
        }
        
        if (val > max) {
            return max;
        }

        return val;
    }
}
