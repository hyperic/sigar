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
