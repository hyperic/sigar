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

import java.util.Date;
import java.text.SimpleDateFormat;

import org.hyperic.sigar.SigarException;

public class Who extends SigarCommandBase {

    public Who(Shell shell) {
        super(shell);
    }

    public Who() {
        super();
    }

    public String getUsageShort() {
        return "Show who is logged on";
    }

    private String getTime(long time) {
        if (time == 0) {
            return "unknown";
        }
        String fmt = "MMM dd HH:mm";
        return new SimpleDateFormat(fmt).format(new Date(time));
    }

    public void output(String[] args) throws SigarException {
        org.hyperic.sigar.Who[] who = this.sigar.getWhoList();
        for (int i=0; i<who.length; i++) {
            String host = who[i].getHost();
            if (host.length() != 0) {
                host = "(" + host + ")";
            }
            printf(new String[] {
                who[i].getUser(),
                who[i].getDevice(),
                getTime(who[i].getTime() * 1000),
                host            
            });
        }
    }

    public static void main(String[] args) throws Exception {
        new Who().processCommand(args);
    }
}
