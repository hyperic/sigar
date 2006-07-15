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

public interface ShellCommandHandler {

    /**
     * Initialize this command handler.
     * @param commandName The name of the command.
     * @param shell The shell.  This is useful for command
     * that need to be able to interpret other commands, like the "help" 
     * command, and for commands that need to get additional user input,
     * for example a login command that presents a password prompt.
     */
    public void init(String commandName, ShellBase shell)
        throws ShellCommandInitException;

    /**
     * Handle a command.
     * @param args The args to the command.
     * @exception ShellCommandUsageException If the args are malformed.
     * @exception ShellCommandExecException If an error occurred
     * executing the command.
     */
    public void processCommand(String[] args)
        throws ShellCommandUsageException, ShellCommandExecException;

    /**
     * Get some info on how to invoke this command.
     * @return Some usage information on how this command is 
     * expected to be invoked.
     */
    public String getUsageHelp(String[] args);

    /**
     * Get a very brief (40 character) description of the command
     * @return A description of the command.
     */
    public String getUsageShort();

    /**
     * Get a description of the syntax for how a command should be invoked.
     * @return A description of the syntax
     */
    public String getSyntax();
}
