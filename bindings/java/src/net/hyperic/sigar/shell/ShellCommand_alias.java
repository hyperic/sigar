package net.hyperic.sigar.shell;

import java.util.HashMap;
import java.util.Iterator;

public class ShellCommand_alias extends ShellCommandBase {

    private static HashMap aliases = new HashMap();

    public static String[] getAlias(String alias) {
        return (String[])aliases.get(alias);
    }

    public static Iterator getAliases() {
        return aliases.keySet().iterator();
    }

    public void processCommand(String[] args)
        throws ShellCommandUsageException, ShellCommandExecException 
    {
        if (args.length < 2) {
            throw new ShellCommandUsageException(this.getSyntax());
        }

        int aliasArgsLen = args.length - 1;
        String[] aliasArgs = new String[ aliasArgsLen ];
        System.arraycopy(args, 1, aliasArgs, 0, aliasArgsLen);

        aliases.put(args[0], aliasArgs);
    }

    public String getSyntaxArgs() {
        return "<alias> <command>";
    }

    public String getUsageShort() {
        return "Create alias command";
    }

    public String getUsageHelp(String[] args) {
        if (aliases.size() == 0) {
            return "No aliases defined";
        }

        StringBuffer sb = new StringBuffer();
        sb.append("Defined aliases:\n");

        for (Iterator it=aliases.keySet().iterator();
             it.hasNext(); )
        {
            String key = (String)it.next();
            String[] cmd = getAlias(key);
            sb.append(key).append(" => ");
            for (int i=0; i<cmd.length; i++) {
                sb.append(cmd[i]).append(" ");
            }
            sb.append("\n");
        }

        return sb.toString();
    }
}
