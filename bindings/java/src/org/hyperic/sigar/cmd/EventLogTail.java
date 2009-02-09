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

import org.hyperic.sigar.win32.EventLog;
import org.hyperic.sigar.win32.EventLogNotification;
import org.hyperic.sigar.win32.EventLogRecord;
import org.hyperic.sigar.win32.EventLogThread;
import org.hyperic.sigar.win32.Win32Exception;

public class EventLogTail {

    private static void tail(String name, Tail tail) throws Win32Exception {
        EventLog log = new EventLog();
        log.open(name);
        int max = log.getNumberOfRecords();
        if (tail.number < max) {
            max = tail.number;
        }
        int last = log.getNewestRecord()+1;
        int first = last - max;

        for (int i=first; i<last; i++) {
            EventLogRecord record = log.read(i);
            System.out.println(record);
        }
        log.close();
    }

    private static class TailNotification implements EventLogNotification {
        public void handleNotification(EventLogRecord event) {
            System.out.println(event);
        }

        public boolean matches(EventLogRecord event) {
            return true;
        }
    }

    public static void main(String[] args) throws Exception {
        Tail tail = new Tail();
        tail.parseArgs(args);

        if (tail.files.size() == 0) {
            tail.files.add(EventLog.SYSTEM);
        }

        for (int i=0; i<tail.files.size(); i++) {
            String name = (String)tail.files.get(i);
            tail(name, tail);

            if (tail.follow) {
                TailNotification notifier = new TailNotification();
                EventLogThread thread = 
                    EventLogThread.getInstance(name);
                thread.add(notifier);
                thread.doStart();
            }
        }

        if (tail.follow) {
            System.in.read();
        }
    }
}
