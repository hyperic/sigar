package net.hyperic.sigar.shell;

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
