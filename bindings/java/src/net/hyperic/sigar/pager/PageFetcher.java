package net.hyperic.sigar.pager;

/**
 * A class which abstracts the fetching of the data for the pages 
 * so callers can deal directly with a single object.
 */

public abstract class PageFetcher {
    /**
     * Get a page of data, as specified by the control.
     */
    public abstract PageList getPage(PageControl control)
        throws PageFetchException;
}
