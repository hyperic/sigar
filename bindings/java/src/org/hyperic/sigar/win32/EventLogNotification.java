package org.hyperic.sigar.win32;

/**
 * Register for event log notifications.
 */
public interface EventLogNotification {

    /**
     * Determine if we want to handle this event.
     */
    public abstract boolean matches(EventLogRecord event);

    /**
     * Called if matches() returns true
     */
    public abstract void handleNotification(EventLogRecord event);
}
