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

/**
 * The default page processor does not process any elements
 * that you're paging.  This is useful if you're only looking
 * to page through an existing collection, and you don't need
 * to perform any transformations on the elements that are
 * found to belong in the resultant page.
 */
public class DefaultPagerProcessor implements PagerProcessor {

    /**
     * Default processor does not process anything, it just
     * returns what was passed in.
     * @param o The object to process.
     * @return The same (completely unmodified) object that was passed in.
     * @see PagerProcessor#processElement
     */
    public Object processElement(Object o) {
        return o;
    }
}
