package net.hyperic.sigar.shell;

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
