package net.hyperic.sigar;

/**
 * CPU percentage usage
 */
public class CpuPerc {
    private double user;
    private double sys;
    private double nice;
    private double idle;

    CpuPerc(){ }

    static CpuPerc calculate(Cpu oldCpu, Cpu curCpu) {
        double diffUser, diffSys, diffNice, diffIdle, diffTotal;

        diffUser = curCpu.getUser() - oldCpu.getUser();
        diffSys  = curCpu.getSys()  - oldCpu.getSys();
        diffNice = curCpu.getNice() - oldCpu.getNice();
        diffIdle = curCpu.getIdle() - oldCpu.getIdle();

        // Sanity check -- sometimes these values waiver in between
        // whole numbers when Cpu is checked very rapidly
        diffUser = diffUser < 0 ? 0 : diffUser;
        diffSys  = diffSys  < 0 ? 0 : diffSys;
        diffNice = diffNice < 0 ? 0 : diffNice;
        diffIdle = diffIdle < 0 ? 0 : diffIdle;

        diffTotal = diffUser + diffSys + diffNice + diffIdle;

        CpuPerc perc = new CpuPerc();
        perc.setUser(diffUser / diffTotal);
        perc.setSys(diffSys / diffTotal);
        perc.setNice(diffNice / diffTotal);
        perc.setIdle(diffIdle / diffTotal);
        return perc;
    }

    public double getUser(){
        return this.user;
    }

    void setUser(double user){
        this.user = user;
    }

    public double getSys(){
        return this.sys;
    }

    void setSys(double sys){
        this.sys = sys;
    }

    public double getNice(){
        return this.nice;
    }

    void setNice(double nice){
        this.nice = nice;
    }

    public double getIdle(){
        return this.idle;
    }

    void setIdle(double idle){
        this.idle = idle;
    }

    public double getCombined() {
        return this.user + this.sys;
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
            format(this.idle) + " idle";
    }
}
