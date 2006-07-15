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

import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.util.PrintfFormat;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Display how long the system has been running and the current load averages.
 */
public class Uptime extends SigarCommandBase {

    private static Object[] loadAvg = new Object[3];

    private static PrintfFormat formatter =
        new PrintfFormat("%.2f, %.2f, %.2f");

    public Uptime(Shell shell) {
        super(shell);
    }

    public Uptime() {
        super();
    }

    public String getUsageShort() {
        return "Display how long the system has been running";
    }

    public void output(String[] args) throws SigarException {
        System.out.println(getInfo(this.sigar));
    }

    public static String getInfo(SigarProxy sigar) throws SigarException {
        double uptime = sigar.getUptime().getUptime();

        String loadAverage;

        try {
            double[] avg = sigar.getLoadAverage();
            loadAvg[0] = new Double(avg[0]);
            loadAvg[1] = new Double(avg[1]);
            loadAvg[2] = new Double(avg[2]);

            loadAverage = "load average: " +
                formatter.sprintf(loadAvg);

        } catch (SigarNotImplementedException e) {
            loadAverage = "(load average unknown)";
        }

        return
            "  " + getCurrentTime() + 
            "  up " + formatUptime(uptime) +
            ", " + loadAverage;
    }

    private static String formatUptime(double uptime) {
        String retval = "";

        int days = (int)uptime / (60*60*24);
        int minutes, hours;

        if (days != 0) {
            retval += days + " " + ((days > 1) ? "days" : "day") + ", ";
        }

        minutes = (int)uptime / 60;
        hours = minutes / 60;
        hours %= 24;
        minutes %= 60;

        if (hours != 0) {
            retval += hours + ":" + minutes;
        }
        else {
            retval += minutes + " min";
        }

        return retval;
    }

    private static String getCurrentTime() {
        return new SimpleDateFormat("h:mm a").format(new Date());
    }

    //pretty close to uptime command, but we don't output number of users yet
    public static void main(String[] args) throws Exception {
        new Uptime().processCommand(args);
    }
}
