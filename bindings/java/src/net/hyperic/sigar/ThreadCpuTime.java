package net.hyperic.sigar;

public class ThreadCpuTime extends ThreadCpu {
    private ThreadCpu diff = null;
    private Sigar sigar;

    public ThreadCpuTime(Sigar sigar) {
        super();
        this.sigar = sigar;
    }

    public void getCurrent() throws SigarException {
        this.gather(this.sigar, 0);
    }

    public ThreadCpu getDiff() throws SigarException {
        long startTotal = this.total;
        long startUser  = this.user;
        long startSys   = this.sys;
    
        if (this.diff == null) {
            this.diff = new ThreadCpu();
        }

        getCurrent();
        
        this.diff.total = this.total - startTotal;
        this.diff.user  = this.user  - startUser;
        this.diff.sys   = this.sys   - startSys;
        
        return this.diff;
    }
}
