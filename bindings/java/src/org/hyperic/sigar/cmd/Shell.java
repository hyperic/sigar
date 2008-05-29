/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

package org.hyperic.sigar.cmd;

import java.io.IOException;
import java.io.File;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarPermissionDeniedException;
import org.hyperic.sigar.SigarLoader;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarProxyCache;

import org.hyperic.sigar.ptql.ProcessFinder;

import org.hyperic.sigar.shell.ShellBase;
import org.hyperic.sigar.shell.ShellCommandExecException;
import org.hyperic.sigar.shell.ShellCommandHandler;
import org.hyperic.sigar.shell.ShellCommandInitException;
import org.hyperic.sigar.shell.ShellCommandUsageException;

import org.hyperic.sigar.test.SigarTestCase;
import org.hyperic.sigar.test.SigarTestRunner;

import org.hyperic.sigar.util.Getline;

/**
 * The Sigar Shell provides a command shell for running the example
 * commands and Sigar tests.
 */
public class Shell extends ShellBase {

    public static final String RCFILE_NAME = ".sigar_shellrc";
    private static final String CLEAR_SCREEN = "\033[2J";

    private Sigar sigar = new Sigar();
    private SigarProxy proxy = SigarProxyCache.newInstance(this.sigar);
    private long[] foundPids = new long[0];
    private boolean isInteractive = false;

    public Shell() {
    }

    public static void clearScreen() {
        System.out.print(CLEAR_SCREEN);
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

    public void setInteractive(boolean value) {
        this.isInteractive = value;
    }

    public void registerCommands() throws ShellCommandInitException {
        registerCommandHandler("df", new Df(this));
        registerCommandHandler("du", new Du(this));
        registerCommandHandler("ls", new Ls(this));
        registerCommandHandler("iostat", new Iostat(this));
        registerCommandHandler("free", new Free(this));
        registerCommandHandler("pargs", new ShowArgs(this));
        registerCommandHandler("penv", new ShowEnv(this));
        registerCommandHandler("pfile", new ProcFileInfo(this));
        registerCommandHandler("pmodules", new ProcModuleInfo(this));
        registerCommandHandler("pinfo", new ProcInfo(this));
        registerCommandHandler("cpuinfo", new CpuInfo(this));
        registerCommandHandler("ifconfig", new Ifconfig(this));
        registerCommandHandler("uptime", new Uptime(this));
        registerCommandHandler("ps", new Ps(this));
        registerCommandHandler("pidof", new Pidof(this));
        registerCommandHandler("kill", new Kill(this));
        registerCommandHandler("netstat", new Netstat(this));
        registerCommandHandler("netinfo", new NetInfo(this));
        registerCommandHandler("nfsstat", new Nfsstat(this));
        registerCommandHandler("route", new Route(this));
        registerCommandHandler("version", new Version(this));
        registerCommandHandler("mps", new MultiPs(this));
        registerCommandHandler("sysinfo", new SysInfo(this));
        registerCommandHandler("time", new Time(this));
        registerCommandHandler("ulimit", new Ulimit(this));
        registerCommandHandler("who", new Who(this));
        if (SigarLoader.IS_WIN32) {
            registerCommandHandler("service", new Win32Service(this));
            registerCommandHandler("fversion", new FileVersionInfo(this));
        }
        try {
            //requires junit.jar
            registerCommandHandler("test", new SigarTestRunner(this));
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

    public long[] findPids(String query) throws SigarException {
        return findPids(new String[] { query });
    }

    public void readCommandFile(String dir) {
        try {
            File rc = new File(dir, RCFILE_NAME);
            readRCFile(rc, false);
            if (this.isInteractive && Getline.isTTY()) {
                this.out.println("Loaded rc file: " + rc);
            }
        } catch (IOException e) { }
    }

    public String getUserDeniedMessage(long pid) {
        return
            SigarPermissionDeniedException.getUserDeniedMessage(this.proxy,
                                                                pid);
    }

    public void shutdown() {
        this.sigar.close();
        //avoid possible Class Not Found: junit/framework/TestCase
        if (System.getProperty("jni.dmalloc") != null) {
            SigarTestCase.closeSigar(); //shutup dmalloc
        }
        super.shutdown();
    }

    public static void main(String[] args) {
        Shell shell = new Shell();

        try {
            if (args.length == 0) {
                shell.isInteractive = true;
            }

            shell.init("sigar", System.out, System.err);
            shell.registerCommands();

            shell.readCommandFile(System.getProperty("user.home"));
            shell.readCommandFile(".");
            shell.readCommandFile(SigarLoader.getLocation());

            if (shell.isInteractive) {
                shell.initHistory();
                Getline.setCompleter(shell);
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
