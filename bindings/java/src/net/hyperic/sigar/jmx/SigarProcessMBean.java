package net.hyperic.sigar.jmx;

/*
 * yeah, yeah, we could generate this class via xdoclet,
 * whoopdi-friggin-do... by hand is much less pain.
 */

public interface SigarProcessMBean {

    public Long getMemSize();

    /**
     * @deprecated
     * @see getMemSize
     */
    public Long getMemVsize();

    public Long getMemShare();

    public Long getTimeUser();

    public Long getTimeSys();
}
