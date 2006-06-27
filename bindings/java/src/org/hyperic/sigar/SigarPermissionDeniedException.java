package org.hyperic.sigar;

public class SigarPermissionDeniedException extends SigarException {

    public static String getUserDeniedMessage(SigarProxy sigar, long pid) {
        String user="unknown", owner="unknown";

        try {
            user = sigar.getProcCredName(sigar.getPid()).getUser();
        } catch (SigarException e) { }

        try {
            owner = sigar.getProcCredName(pid).getUser();
        } catch (SigarException e) { }

        return
            "User " + user + " denied access to process "
            + pid + " owned by " + owner;
    }

    public SigarPermissionDeniedException(String s) { super(s); }

    public SigarPermissionDeniedException(SigarProxy sigar, long pid) {
        super(getUserDeniedMessage(sigar, pid));
    }
}
