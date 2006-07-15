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

public class ShellCommand_get extends ShellCommandBase {

    private void printProperty(String key, String value) {
        PrintStream out = getOutStream();

        out.print(key + "=");

        if (value.trim() != value) {
            out.println("'" + value + "'");
        }
        else {
            out.println(value);
        }
    }

    public void processCommand(String[] args)
        throws ShellCommandUsageException, ShellCommandExecException 
    {
        if (args.length < 1) {
            throw new ShellCommandUsageException(this.getSyntax());
        }

        for (int i=0; i<args.length; i++) {
            String val = System.getProperty(args[i], "UNDEFINED");

            printProperty(args[i], val);
        }
    }

    public String getSyntaxArgs() {
        return "<key1> [key2] ...";
    }

    public String getUsageShort(){
        return "Get system properties";
    }

    public String getUsageHelp(String[] args) {
        return "    " + getUsageShort() + ".";
    }
}
