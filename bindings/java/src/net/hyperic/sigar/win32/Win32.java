package net.hyperic.sigar.win32;

import java.io.File;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;

public abstract class Win32 {

    static {
        try {
            Sigar.load();
        } catch (SigarException e) {
            
        }
    }

    public static native String findExecutable(String name)
        throws SigarException;

    public static String findScriptExecutable(String name) {
        int ix = name.lastIndexOf(".");
        if (ix == -1) {
            return null;
        }

        String ext = name.substring(ix+1);
        if (ext.equals("exe") ||
            ext.equals("bat") ||
            ext.equals("com"))
        {
            return null;
        }

        String exe;
        try {
            exe = findExecutable(new File(name).getAbsolutePath());
        } catch (SigarException e) {
            return null;
        }
        if (exe == null) {
            return null; //no association
        }

        exe = exe.toLowerCase();
        name = name.toLowerCase();
        if (exe.equals(name) || exe.endsWith(name)) {
            return null; //same thing
        }

        File file = new File(exe);
        //rewrite to use cscript for command line stuff
        if (file.getName().equals("wscript.exe")) {
            exe =
                file.getParent() +
                File.separator +
                "cscript.exe";
        }

        return exe;
    }

    public static void main(String[] args) throws Exception {
        for (int i=0; i<args.length; i++) {
            String file =
                new File(args[i]).getAbsoluteFile().toString();
            String exe =
                findScriptExecutable(file);
            if (exe == null) {
                continue;
            }
            System.out.println(args[i] + "=" + exe);
        }
    }
}
