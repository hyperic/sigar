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

package org.hyperic.sigar.win32;

import java.util.Date;

/**
 * Class to represent event log records
 */
public class EventLogRecord {

    long recordNumber;
    long timeGenerated; 
    long timeWritten;
    long eventId;

    short eventType;

    String source;
    String computerName;
    String user;
    String message;
    String logName;

    EventLogRecord() {}

    /**
     * @return Event log name which generated the event
     */
    public String getLogName() {
        return this.logName;
    }

    void setLogName(String logName) {
        this.logName = logName;
    }

    /* Get the record number for this event entry */
    public long getRecordNumber() {
        return recordNumber;
    }

    /**
     * Get the time at which this entry was submitted. This time is 
     * measured in the number of seconds elapsed since 00:00:00 
     * January 1, 1970, Universal Coordinated Time. 
     */

    public long getTimeGenerated() {
        return this.timeGenerated;
    }

    /**
     * Get the time at which this entry was received by the service to be 
     * written to the logfile. This time is measured in the number of 
     * seconds elapsed since 00:00:00 January 1, 1970, Universal 
     * Coordinated Time.
     */
    public long getTimeWritten() {
        return this.timeWritten;
    }

    /**
     * Event identifier. The value is specific to the event source 
     * for the event, and is used with source name to locate a 
     * description string in the message file for the event source. 
     *
     * XXX: This is probably not needed
     */
    public long getEventId() {
        return this.eventId;
    }

    /**
     * Return the event type.  
     * See the EVENTLOG_* constants in the EventLog class
     */
    public short getEventType() {
        return this.eventType;
    }

    public String getEventTypeString() {
        switch (this.eventType) {
            case EventLog.EVENTLOG_ERROR_TYPE:
                return "Error";
            case EventLog.EVENTLOG_WARNING_TYPE:
                return "Warning";
            case EventLog.EVENTLOG_INFORMATION_TYPE:
                return "Information";
            case EventLog.EVENTLOG_AUDIT_SUCCESS:
                return "Success Audit";
            case EventLog.EVENTLOG_AUDIT_FAILURE:
                return "Failure Audit";
            default:
                return "Unknown";
        }
    }

    /**
     * Get the application which triggered the event
     */
    public String getSource() {
        return this.source;
    }

    /**
     * Get the machine name where the event was generated
     */
    public String getComputerName() {
        return this.computerName;
    }

    /**
     * Get the user who generated the event.  May be null if no user is
     * associated with the event.
     */
    public String getUser() {
        return this.user;
    }
    
    /**
     * Get the message for the event.
     */
    public String getMessage() {
        return this.message;
    }

    /**
     * @deprecated
     */
    public String getStringData() {
        return getMessage();
    }

    /**
     * For debugging
     */
    public String toString()
    {
        return
        "[" + new Date(getTimeGenerated() * 1000) + "] " +
        "[" + getEventTypeString() + "] " +
        "[" + getSource() + "] " +
        getMessage();
    }
}

