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

import java.util.HashMap;
import java.util.Iterator;

public class ShellCommand_set extends ShellCommandBase {
    private HashMap keyDescriptions = new HashMap();

    public ShellCommand_set() {
        this.keyDescriptions = new HashMap();
        this.keyDescriptions.put(ShellBase.PROP_PAGE_SIZE,
                                 "The maximum size of a shell page");
    }

    public void processCommand(String[] args)
        throws ShellCommandUsageException, ShellCommandExecException 
    {
        if (args.length < 1 || args.length > 2) {
            throw new ShellCommandUsageException(this.getSyntax());
        }

        if (args.length == 1) {
            System.getProperties().remove(args[0]);
        }
        else {
            if (args[0].equalsIgnoreCase(ShellBase.PROP_PAGE_SIZE)) {
                int newSize;

                try {
                    newSize = Integer.parseInt(args[1]);
                    if (newSize == 0 || newSize < -1) {
                        throw new NumberFormatException();
                    }
                } catch(NumberFormatException exc) {
                    throw new ShellCommandUsageException(args[0] + " must be "+
                                                         "an integer > 0 or " +
                                                         "-1");
                }
                this.getShell().setPageSize(newSize);
            } 

            System.setProperty(args[0], args[1]);
        }
    }

    public void addSetKey(String key, String description) {
        this.keyDescriptions.put(key, description);
    }

    public String getSyntaxArgs() {
        return "<key> [value]";
    }

    public String getUsageShort() {
        return "Set system properties";
    }

    public String getUsageHelp(String[] args) {
        String res =
            "    " + this.getUsageShort() +
            ".  If no value is provided, " +
            "the key will be\n    deleted.";

        if (this.keyDescriptions.size() != 0) {
            res += "\n\n    Common keys include:";
        }

        for (Iterator i=this.keyDescriptions.keySet().iterator();
             i.hasNext();)
        {
            String key = (String)i.next();
            String value = (String)this.keyDescriptions.get(key);

            res += "\n      " + key + ": " + value;
        }

        return res;
    }
}
