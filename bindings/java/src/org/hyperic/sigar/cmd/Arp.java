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

import org.hyperic.sigar.SigarException;

public class Arp extends SigarCommandBase {
    public Arp(Shell shell) {
        super(shell);
    }

    public Arp() {
        super();
    }

    public String getUsageShort() {
        return "Display system ARP cache";
    }

    //arp -an
    public void output(String[] args) throws SigarException {
        org.hyperic.sigar.Arp[] entries = this.sigar.getArpList();

        for (int i=0; i<entries.length; i++) {
            org.hyperic.sigar.Arp arp = entries[i];
            String host = "?"; //XXX
            println(host + " " +
                    "(" + arp.getAddress() + ")" + " at " +
                    arp.getHwaddr() + " " +
                    "[" + arp.getType() + "]" + " on " +
                    arp.getIfname());
        }
    }

    public static void main(String[] args) throws Exception {
        new Arp().processCommand(args);
    }
}
