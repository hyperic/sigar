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

package org.hyperic.sigar.cmd;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.win32.Service;
import org.hyperic.sigar.win32.Win32Exception;

public class Win32Service extends SigarCommandBase {

    private static final List COMMANDS = 
        Arrays.asList(new String[] {
            "state",
            "start",
            "stop",
            "pause",
            "resume",
            "restart",
        });
    
    public Win32Service() {
        super();
    }

    public Win32Service(Shell shell) {
        super(shell);
    }

    public String getSyntaxArgs() {
        return "[name] [action]";
    }

    public String getUsageShort() {
        return "Windows service commands";
    }

    protected boolean validateArgs(String[] args) {
        return (args.length == 1) || (args.length == 2);
    }

    public Collection getCompletions() {
        try {
            return Service.getServiceNames();
        } catch (Win32Exception e) {
            return null;
        }
    }

    public void output(String[] args) throws SigarException {
        Service service = null;
        String name = args[0];
        String cmd = null;

        if (args.length == 2) {
            cmd = args[1];
        }
        
        try {
            service = new Service(name);
        
            if ((cmd == null) || cmd.equals("state")) {
                service.list(this.out);
            }
            else if (cmd.equals("start")) {
                service.start();
            }
            else if (cmd.equals("stop")) {
                service.stop();
            }
            else if (cmd.equals("pause")) {
                service.pause();
            }
            else if (cmd.equals("resume")) {
                service.resume();
            }
            else if (cmd.equals("delete")) {
                service.delete();
            }
            else if (cmd.equals("restart")) {
                service.stop(0);
                service.start();
            }
            else {
                println("Unsupported service command: " + args[1]);
                println("Valid commands: " + COMMANDS);
            }
        } finally {
            if (service != null) {
                service.close();
            }
        }
    }
}
