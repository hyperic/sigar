package net.hyperic.sigar.cmd;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.lang.reflect.Method;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarProxy;

import net.hyperic.sigar.util.GetlineCompleter;

import net.hyperic.sigar.ptql.ProcessQueryBuilder;

import net.hyperic.sigar.shell.CollectionCompleter;
import net.hyperic.sigar.shell.MultiwordShellCommand;
import net.hyperic.sigar.shell.ShellBase;
import net.hyperic.sigar.shell.ShellCommandExecException;
import net.hyperic.sigar.shell.ShellCommandInitException;
import net.hyperic.sigar.shell.ShellCommandHandler;
import net.hyperic.sigar.shell.ShellCommandUsageException;

public class PTQL
    extends MultiwordShellCommand
    implements GetlineCompleter {

    private Shell shell;
    private PrintStream out = System.out;
    private Sigar sigar;
    private SigarProxy proxy;
    private long[] foundPids = null;
    private Ps ps;
    private GetlineCompleter m_completer;
    private Map methods;

    private PTQL() { }

    public PTQL(Shell shell) {
        this.shell = shell;
        this.out   = shell.getOutStream();
        this.sigar = shell.getSigar();
        this.proxy = shell.getSigarProxy();
        this.ps    = new Ps(this.shell);
        this.methods = ProcessQueryBuilder.getMethods();
        this.m_completer =
            new CollectionCompleter(shell, methods.keySet());
    }

    public String complete(String line) {
        int ix = line.indexOf(".");

        if (ix == -1) {
            line = this.m_completer.complete(line);
            if (!line.endsWith(".")) {
                if (this.methods.get(line) != null) {
                    return line + ".";
                }
            }
            return line;
        }

        String attrClass = line.substring(0, ix);
        String attr = line.substring(ix+1, line.length());

        Method method = (Method)this.methods.get(attrClass);
        Class subtype = method.getReturnType();

        if (method == null) {
            return line;
        }

        boolean isSigarClass = ProcessQueryBuilder.isSigarClass(subtype);

        int ix2 = attr.indexOf(".");
        if (ix2 != -1) {
            method = null;
            String op = attr.substring(ix2+1, attr.length());
            attr = attr.substring(0, ix2);

            if (isSigarClass) {
                try {
                    method =
                        subtype.getMethod("get" + attr,
                                          ProcessQueryBuilder.NOPARAM);
                } catch (NoSuchMethodException e) { }
            }

            final Method m = method;

            GetlineCompleter completer =
                new CollectionCompleter(this.shell,
                                        ProcessQueryBuilder.
                                        getMethodOpNames(m));

            String partial = completer.complete(op);
            String result = attrClass + "." + attr + "." + partial;
            if (partial.length() == 2) {
                result += "=";
            }
            return result;
        }

        if (isSigarClass) {
            final ArrayList possible = new ArrayList();
            Method[] submethods = subtype.getDeclaredMethods();
            for (int i=0; i<submethods.length; i++) {
                Method m = submethods[i];
                if (m.getName().startsWith("get")) {
                    possible.add(m.getName().substring(3));
                }
            }

            GetlineCompleter completer =
                new CollectionCompleter(this.shell, possible);

            String partial = completer.complete(attr);
            String result = attrClass + "." + partial;
            if (possible.contains(partial)) {
                result += ".";
            }
            return result;
        }
        
        return line;
    }

    public void init(String commandName, ShellBase shell)
        throws ShellCommandInitException
    {
        ShellCommandHandler handler;

        super.init(commandName, shell);

        handler = new ProcessQueryGenerate(this.shell);
        registerSubHandler("generate", handler);
    }

    public String getSyntaxArgs() {
        return "<query>";
    }

    public String getUsageShort() {
        return "Run process table query";
    }

    public void processCommand(String[] args) 
        throws ShellCommandUsageException, ShellCommandExecException 
    {
        if (args.length > 0) {
            if (getSubHandler(args[0]) != null) {
                super.processCommand(args);
                return;
            }
        }

        if (args.length != 1) {
            throw new ShellCommandUsageException(getSyntax());
        }

        long[] pids;
        try {
            pids = this.shell.findPids(args);
        } catch (NumberFormatException e) {
            throw new ShellCommandUsageException(getSyntax());
        } catch (SigarException e) {
            throw new ShellCommandExecException(e.getMessage());
        }

        for (int i=0; i<pids.length; i++) {
            try {
                this.ps.output(pids[i]);
            } catch (SigarException e) {
                throw new ShellCommandExecException(e.getMessage());
            }
        }

        this.ps.flush();
    }
}
