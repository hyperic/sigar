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

import java.util.ArrayList;
import java.util.Map;
import java.lang.reflect.Method;

import org.hyperic.sigar.SigarException;

import org.hyperic.sigar.util.GetlineCompleter;

import org.hyperic.sigar.ptql.ProcessQueryBuilder;

import org.hyperic.sigar.shell.CollectionCompleter;
import org.hyperic.sigar.shell.MultiwordShellCommand;
import org.hyperic.sigar.shell.ShellBase;
import org.hyperic.sigar.shell.ShellCommandExecException;
import org.hyperic.sigar.shell.ShellCommandInitException;
import org.hyperic.sigar.shell.ShellCommandHandler;
import org.hyperic.sigar.shell.ShellCommandUsageException;

/**
 * Run process table queries.
 * @see org.hyperic.sigar.ptql.ProcessQueryBuilder
 */
public class PTQL
    extends MultiwordShellCommand
    implements GetlineCompleter {

    private Shell shell;
    private Ps ps;
    private GetlineCompleter m_completer;
    private Map methods;

    private PTQL() { }

    public PTQL(Shell shell) {
        this.shell = shell;
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
        if (method == null) {
            return line;
        }

        Class subtype = method.getReturnType();

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
