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

import java.util.Arrays;
import java.util.List;

/**
 * A fetcher which uses a static array of strings to page
 * through.
 */

public class StaticPageFetcher extends PageFetcher {

    private List data;

    public StaticPageFetcher(String[] data) {
        this.data = Arrays.asList(data);
    }

    public StaticPageFetcher(List data) {
        this.data = data;
    }

    public PageList getPage(PageControl control)
        throws PageFetchException
    {
        PageList res = new PageList();
        int startIdx, endIdx;

        res.setTotalSize(this.data.size());

        if (control.getPagesize() == PageControl.SIZE_UNLIMITED ||
            control.getPagenum() == -1)
        {
            res.addAll(this.data);
            return res;
        }

        startIdx = control.getPageEntityIndex();
        endIdx   = startIdx + control.getPagesize();

        if (startIdx >= this.data.size()) {
            return res;
        }

        if (endIdx > this.data.size()) {
            endIdx = this.data.size();
        }

        res.addAll(this.data.subList(startIdx, endIdx));

        return res;
    }
}
