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
