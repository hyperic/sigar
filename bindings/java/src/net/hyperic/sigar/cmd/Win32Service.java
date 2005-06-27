package net.hyperic.sigar.cmd;

import java.util.Collection;

import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.win32.Service;
import net.hyperic.sigar.win32.Win32Exception;

public class Win32Service extends SigarCommandBase {

    public Win32Service() {
        super();
    }

    public Win32Service(Shell shell) {
        super(shell);
    }

    public String getSyntaxArgs() {
        return "[name] [action]";
    }

    public String getUsageShort() {
        return "Windows service commands";
    }

    protected boolean validateArgs(String[] args) {
        return (args.length == 1) || (args.length == 2);
    }

    public Collection getCompletions() {
        try {
            return Service.getServiceNames();
        } catch (Win32Exception e) {
            return null;
        }
    }

    public void output(String[] args) throws SigarException {
        String name = args[0];
        Service service = new Service(name);
        if (args.length == 1) {
            service.list(this.out);
        }
        else if (args[1].equals("start")) {
            service.start();
        }
        else if (args[1].equals("stop")) {
            service.stop();
        }
        else {
            throw new SigarException("Unsupported service command: " + args[1]);
        }
    }
}
