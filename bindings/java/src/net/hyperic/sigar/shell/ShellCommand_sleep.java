package net.hyperic.sigar.shell;

public class ShellCommand_sleep extends ShellCommandBase {

    public ShellCommand_sleep() {}

    public void processCommand(String[] args)
        throws ShellCommandUsageException, ShellCommandExecException 
    {
        if (args.length != 1) {
            throw new ShellCommandUsageException(getSyntax());
        }

        try {
            Thread.sleep(Integer.parseInt(args[0]) * 1000);
        } catch(NumberFormatException exc) {
            throw new ShellCommandExecException("Invalid time '" + args[0] + 
                                                "' -- must be an integer");
        } catch(InterruptedException exc) {
            throw new ShellCommandExecException("Sleep interrupted");
        }
    }

    public String getSyntaxArgs() {
        return "<numSeconds>";
    }

    public String getUsageShort() {
        return "Delay execution for the a number of seconds ";
    }

    public String getUsageHelp(String[] args) {
        return "    " + getUsageShort() + ".";
    }
}
