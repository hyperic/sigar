package net.hyperic.sigar.shell;

import java.io.File;
import java.io.IOException;

import net.hyperic.sigar.util.GetlineCompleter;

public class ShellCommand_source
    extends ShellCommandBase
    implements GetlineCompleter {

    public String complete(String line) {
        return new FileCompleter(getShell()).complete(line);
    }

    public void processCommand(String[] args) 
        throws ShellCommandUsageException, ShellCommandExecException 
    {
        File rcFile;

        if(args.length != 1){
            throw new ShellCommandUsageException("Syntax: " + 
                                                 this.getCommandName() + 
                                                 " <rcfile>");
        }

        rcFile = new File(FileCompleter.expand(args[0]));
        
        if(rcFile.isFile() == false){
            throw new ShellCommandExecException("File '" + rcFile + 
                                                "' not found");
        }

        try {
            this.getShell().readRCFile(rcFile, true);
        } catch(IOException exc){
            throw new ShellCommandExecException("Error reading file '" + 
                                                rcFile + ": " + 
                                                exc.getMessage());
        }
    }

    public String getSyntaxArgs(){
        return "<rcfile>";
    }

    public String getUsageShort(){
        return "Read a file, executing the contents";
    }

    public String getUsageHelp(String[] args) {
        return "    " + this.getUsageShort() + ".  The file must contain " +
            "commands\n    which are executable by the shell.";
    }
}
