package org.hyperic.sigar.shell;

public class ShellCommand_quit extends ShellCommandBase {
    
    public void processCommand(String[] args) 
        throws ShellCommandUsageException, ShellCommandExecException {
        throw new NormalQuitCommandException();
    }

    public String getUsageShort() {
        return "Terminate the shell";
    }

    public String getUsageHelp(String[] args) {
        return "    Terminate the shell.";
    }
}
