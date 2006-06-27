package org.hyperic.sigar.pager;

/**
 * Provides a point of extensibility in the paging behavior.
 * If you supply a PagerProcessor when you get a Pager,
 * then that processor will be called to process each element
 * as the pager moves it from the source collection to the
 * destination collection.
 */
public interface PagerProcessor {

    /**
     * Process an element as the pager moves it from the source 
     * collection to the destination collection.
     * @param o The object to process.
     * @return The processed object.
     */
    public Object processElement(Object o);
}
