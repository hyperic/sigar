package net.hyperic.sigar.cmd;

import java.io.IOException;
import java.io.File;

import java.util.ArrayList;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarProxyCache;

import net.hyperic.sigar.ptql.ProcessFinder;

import net.hyperic.sigar.shell.ShellBase;
import net.hyperic.sigar.shell.ShellCommandExecException;
import net.hyperic.sigar.shell.ShellCommandHandler;
import net.hyperic.sigar.shell.ShellCommandInitException;
import net.hyperic.sigar.shell.ShellCommandUsageException;

import net.hyperic.sigar.test.SigarTestRunner;

public class Shell extends ShellBase {

    private Sigar sigar = new Sigar();
    private SigarProxy proxy = SigarProxyCache.newInstance(this.sigar);
    private ProcessFinder finder = new ProcessFinder(this.proxy);
    private long[] foundPids = new long[0];
    private ArrayList completions = new ArrayList();
    private boolean isInteractive = false;

    public Shell() {
    }

    public SigarProxy getSigarProxy() {
        return this.proxy;
    }

    public Sigar getSigar() {
        return this.sigar;
    }

    public boolean isInteractive() {
        return this.isInteractive;
    }

    public void registerCommands() throws ShellCommandInitException {
        registerCommandHandler("df", new Df(this));
        registerCommandHandler("free", new Free(this));
        registerCommandHandler("pargs", new ShowArgs(this));
        registerCommandHandler("penv", new ShowEnv(this));
        registerCommandHandler("pfile", new ProcFileInfo(this));
        registerCommandHandler("pmodules", new ProcModuleInfo(this));
        registerCommandHandler("cpuinfo", new CpuInfo(this));
        registerCommandHandler("ifconfig", new Ifconfig(this));
        registerCommandHandler("uptime", new Uptime(this));
        registerCommandHandler("ps", new Ps(this));
        registerCommandHandler("kill", new Kill(this));
        registerCommandHandler("netstat", new Netstat(this));
        registerCommandHandler("version", new Version(this));
        try {
            //requires junit.jar
            registerCommandHandler("test", new SigarTestRunner(this));
            //requires bcel-5.1.jar
            registerCommandHandler("ptql", new PTQL(this));
        } catch (NoClassDefFoundError e) { }
    }

    public void processCommand(ShellCommandHandler handler, String args[])
        throws ShellCommandUsageException, ShellCommandExecException
    {
        try {
            super.processCommand(handler, args);
            if (handler instanceof SigarCommandBase) {
                ((SigarCommandBase)handler).flush();
            }
        } finally {
            SigarProxyCache.clear(this.proxy);
        }
    }

    public static long[] getPids(SigarProxy sigar, String[] args)
        throws SigarException {

        long[] pids;

        switch (args.length) {
          case 0:
            pids = new long[] { sigar.getPid() };
            break;
          case 1:
            if (args[0].indexOf("=") > 0) {
                pids = ProcessFinder.find(sigar, args[0]);
            }
            else if (args[0].equals("$$")) {
                pids = new long[] { sigar.getPid() };
            }
            else {
                pids = new long[] {
                    Long.parseLong(args[0])
                };
            }
            break;
          default:
            pids = new long[args.length];
            for (int i=0; i<args.length; i++) {
                pids[i] = Long.parseLong(args[i]);
            }
            break;
        }

        return pids;
    }

    public long[] findPids(String[] args) throws SigarException {

        if ((args.length == 1) && args[0].equals("-")) {
            return this.foundPids;
        }

        this.foundPids = getPids(this.proxy, args);

        return this.foundPids;
    }

    public static void main(String[] args) {
        Shell shell = new Shell();

        try {
            shell.init("sigar", System.out, System.err);
            shell.registerCommands();
            String home = System.getProperty("user.home");
            try {
                shell.readRCFile(new File(home, ".sigar"), false);
            } catch (IOException e) { }

            if (args.length == 0) {
                shell.isInteractive = true;
                shell.initHistory();
                shell.gl.setCompleter(shell);
                shell.run();
            }
            else {
                shell.handleCommand(null, args);
            }
        } catch (Exception e) {
            System.err.println("Unexpected exception: " + e);
        } finally {
            shell.shutdown();
        }
    }
}
