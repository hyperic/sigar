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

package org.hyperic.sigar.shell;

import java.io.PrintStream;

public class ShellCommandBase implements ShellCommandHandler {

    protected String itsCommandName = null;
    protected ShellBase itsShell = null;

    private PrintStream out = null;
    public String getCommandName() { return itsCommandName; }
    public ShellBase getShell() { return itsShell; }

    public PrintStream getOutStream() { 
        return this.getShell().getOutStream(); 
    }

    public PrintStream getErrStream() { 
        return this.getShell().getErrStream(); 
    }

    public void init(String commandName, ShellBase shell)
        throws ShellCommandInitException {
        itsCommandName = commandName;
        itsShell = shell;
    }

    public void processCommand(String[] args) 
        throws ShellCommandUsageException, ShellCommandExecException {

        out.println("ShellCommandBase: not implemented: " + itsCommandName);
        /*
        if (args != null && args.trim().length() > 0) {
            out.println("args were: " + args);
        }
        */
    }

    public String getSyntax() {
        return "Syntax: " + this.getCommandName() + " " + this.getSyntaxArgs();
    }

    public String getSyntaxArgs() {
        return "";
    }

    public String getUsageShort() {
        return "";
    }

    public String getUsageHelp(String[] args) {
        return "Help not available for command " + itsCommandName;
    }
}
