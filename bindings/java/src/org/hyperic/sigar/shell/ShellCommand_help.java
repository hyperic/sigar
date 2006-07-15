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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

import org.hyperic.sigar.util.PrintfFormat;

public class ShellCommand_help extends ShellCommandBase {
    
    public void processCommand(String[] args) 
        throws ShellCommandUsageException, ShellCommandExecException 
    {
        ShellCommandHandler handler;
        PrintStream out = this.getOutStream();
        int useArgs;

        if (args.length == 0) {
            PrintfFormat fmt = new PrintfFormat("\t%-14s - %s");
            Object[] fArgs = new Object[2];
            ArrayList cmdNamesList = new ArrayList();
            String[] cmdNames;
            Iterator i;

            i = itsShell.getCommandNameIterator();
            while (i.hasNext()) {
                cmdNamesList.add(i.next());
            }
            
            cmdNames = (String[])cmdNamesList.toArray(new String[0]);
            Arrays.sort(cmdNames);
            
            
            out.println("Available commands:");

            for (int j=0; j<cmdNames.length; j++) {
                handler = itsShell.getHandler(cmdNames[j]);
                fArgs[0] = cmdNames[j];
                fArgs[1] = handler.getUsageShort();
                out.println(fmt.sprintf(fArgs));
            }

            return;
        }

        // Sometimes, multi-word shell commands have sub handlers with
        // independent usage messages.  Let's iterate through the
        // command words until we encounter one for which there is no
        // sub handler.  Also ignore any option-words (words beginning
        // with a '-').
        ShellCommandHandler lastHandler = handler =
            itsShell.getHandler(args[0]);

        useArgs = 1;
        for (int i=1; i<args.length; ++i) {
            if (! args[i].startsWith("-")) {
                if (null == handler) {
                    handler = lastHandler;
                    useArgs = i + 1;
                    break;
                }
                else if (handler instanceof MultiwordShellCommand) {
                    MultiwordShellCommand mwsc =
                        (MultiwordShellCommand)handler;
                    lastHandler = handler;
                    handler = mwsc.getSubHandler(args[i]);
                    useArgs = i + 1;
                }
            }
            else {
                break;
            }
        }

        if (handler == null) {
            // Iterate to build the full command-string, ignorign any
            // option arguments.
            out.print("Command '");
            for (int i=0; i<args.length; ++i) {
                if (! args[i].startsWith("-")) {
                    out.print(args[i]);
                    if (i < args.length-1) {
                        out.print(' ');
                    }
                }
                else {
                    break;
                }
            }
            out.println("' not found.");
        } else {
            String[] otherArgs = new String[args.length - useArgs];

            System.arraycopy(args, useArgs, otherArgs, 0, otherArgs.length);
            out.println(handler.getSyntax());
            out.println(handler.getUsageHelp(otherArgs));
        }
    }

    public String getSyntaxArgs() {
        return "<command name> [command arguments]";
    }

    public String getUsageShort() {
        return "Gives help on shell commands";
    }

    public String getUsageHelp(String[] args) {
        return
            "    Displays help about the given command name.  If the \n" +
            "    command has arguments they may be entered for more " +
            "specific\n    help";
    }
}
