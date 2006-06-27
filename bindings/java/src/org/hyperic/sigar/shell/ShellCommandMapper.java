package org.hyperic.sigar.shell;

import java.util.Iterator;

public interface ShellCommandMapper {

    /**
     * Get the command handler for a command.
     */
    public ShellCommandHandler getHandler(String command);

    /**
     * Get an iterator for the command names.
     */
    public Iterator getCommandNameIterator();
}
