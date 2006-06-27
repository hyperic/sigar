package org.hyperic.sigar.pager;

/**
 * This class is useful for classes that implement PagerProcessorExt and 
 * need to do some initialization before paging begins and some cleanup
 * after paging has ended.
 */
public interface PagerEventHandler {

    /** Called before paging begins. */
    public void init();

    /** Called after paging ends. */
    public void cleanup();
}
