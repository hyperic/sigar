package net.hyperic.sigar.win32.test;

import net.hyperic.sigar.win32.EventLog;
import net.hyperic.sigar.win32.EventLogNotification;
import net.hyperic.sigar.win32.EventLogRecord;
import net.hyperic.sigar.win32.EventLogThread;
import net.hyperic.sigar.win32.Win32Exception;


import junit.framework.TestCase;

public class TestEventLog extends TestCase {

    public TestEventLog(String name) {
        super(name);
    }

    public void testOpenClose() throws Exception {

        EventLog log = new EventLog();

        // Try to close an event log that isn't open
        try {
            log.close();
            fail("Closing an unopened event log succeeded");
        } catch (Win32Exception e) {
            // OK
        }

        log.open("Application");
        log.close();

        // Try to reopen using the System log
        log.open("System");
        log.close();
    }

    public void testGetNumberOfRecords() throws Exception {
        int numRecords;
        EventLog log = new EventLog();

        log.open("Application");
        try {
            numRecords = log.getNumberOfRecords();
        } catch (Exception e) {
            fail("Unable to get the number of records");
        }

        log.close();
    }

    public void testGetOldestRecord() throws Exception {
        int oldestRecord;
        EventLog log = new EventLog();

        log.open("Application");
        try {
            oldestRecord = log.getOldestRecord();
        } catch (Exception e) {
            fail("Unable to get the oldest event record");
        }

        log.close();
    }

    public void testGetNewestRecord() throws Exception {
        int newestRecord;
        EventLog log = new EventLog();

        log.open("Application");
        try {
            newestRecord = log.getNewestRecord();
        } catch (Exception e) {
            fail("Unable to get the newest event record");
        }

        log.close();
    }

    // Test reading all records
    public void testRead() throws Exception {
        EventLogRecord record;
        EventLog log = new EventLog();

        log.open("Application");
        int oldestRecord = log.getOldestRecord();
        int numRecords = log.getNumberOfRecords();

        for (int i = oldestRecord; i < oldestRecord + numRecords; i++) {
            record = log.read(i);
        }

        log.close();
    }

    private class SSHEventLogNotification 
        implements EventLogNotification {

        public boolean matches(EventLogRecord record) {
            return record.getSource().equals("sshd");
        }

        public void handleNotification(EventLogRecord record) { 
            System.out.println(record);
        }
    }

    // Test event log thread
    public void testEventLogThread() throws Exception {

        EventLogThread thread =
            EventLogThread.getInstance(EventLog.APPLICATION);

        thread.doStart();

        SSHEventLogNotification notification =
            new SSHEventLogNotification();
        thread.add(notification);

        thread.doStop();
    }
}
