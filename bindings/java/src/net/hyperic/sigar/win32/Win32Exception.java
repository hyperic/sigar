package net.hyperic.sigar.win32;

public class Win32Exception extends Exception {

    private int errorCode;

    public Win32Exception (String s) { super(s); }

    public Win32Exception (int error, String s) {
        super(s);
        this.errorCode = error;
    }

    public int getErrorCode() {
        return this.errorCode;
    }
}
