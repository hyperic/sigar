package net.hyperic.sigar;

/**
 * CPU percentage usage
 */
public class CpuPerc {
    private double user;
    private double sys;
    private double nice;
    private double idle;
    private double wait;

    CpuPerc() {}

    static CpuPerc calculate(Cpu oldCpu, Cpu curCpu) {
        double diffUser, diffSys, diffNice, diffIdle, diffWait, diffTotal;

        diffUser = curCpu.getUser() - oldCpu.getUser();
        diffSys  = curCpu.getSys()  - oldCpu.getSys();
        diffNice = curCpu.getNice() - oldCpu.getNice();
        diffIdle = curCpu.getIdle() - oldCpu.getIdle();
        diffWait = curCpu.getWait() - oldCpu.getWait();

        // Sanity check -- sometimes these values waiver in between
        // whole numbers when Cpu is checked very rapidly
        diffUser = diffUser < 0 ? 0 : diffUser;
        diffSys  = diffSys  < 0 ? 0 : diffSys;
        diffNice = diffNice < 0 ? 0 : diffNice;
        diffIdle = diffIdle < 0 ? 0 : diffIdle;
        diffWait = diffWait < 0 ? 0 : diffWait;

        diffTotal = diffUser + diffSys + diffNice + diffIdle + diffWait;

        CpuPerc perc = new CpuPerc();
        perc.user = diffUser / diffTotal;
        perc.sys  = diffSys / diffTotal;
        perc.nice = diffNice / diffTotal;
        perc.idle = diffIdle / diffTotal;
        perc.wait = diffWait / diffTotal;
        return perc;
    }

    public double getUser() {
        return this.user;
    }

    public double getSys() {
        return this.sys;
    }

    public double getNice() {
        return this.nice;
    }

    public double getIdle() {
        return this.idle;
    }

    public double getWait() {
        return this.wait;
    }

    public double getCombined() {
        return this.user + this.sys + this.nice + this.wait;
    }

    public static String format(double val) {
        String p = String.valueOf(val * 100.0);
        //cant wait for sprintf.
        int ix = p.indexOf(".") + 1;
        String percent =
            p.substring(0, ix) + 
            p.substring(ix, ix+1);
        return percent + "%";
    }

    public String toString() {
        return
            "CPU states: " +
            format(this.user) + " user, " +
            format(this.sys)  + " system, " +
            format(this.nice) + " nice, " +
            format(this.wait) + " wait, " +
            format(this.idle) + " idle";
    }
}
