package net.hyperic.sigar.win32;

public class ServiceConfig {

    // Start type
    /**
     * A device driver started by the system loader. This value is valid only for driver services.
     */
    public static final int START_BOOT   = 0x00000000;
    /**
     * A device driver started by the IoInitSystem function. This value is valid only for driver services.
     */
    public static final int START_SYSTEM   = 0x00000001;
    /**
     * A service started automatically by the service control manager during system startup.
     */
    public static final int START_AUTO     = 0x00000002;
    /**
     * A service started by the service control manager when a process calls the StartService function.
     */
    public static final int START_MANUAL   = 0x00000003;
    /**
     * A service that cannot be started.
     * Attempts to start the service result in the error code ERROR_SERVICE_DISABLED.
     */
    public static final int START_DISABLED = 0x00000004;

    /**
     * Driver service.
     */
    public static final int TYPE_KERNEL_DRIVER       = 0x00000001;
    /**
     * File system driver service.
     */
    public static final int TYPE_FILE_SYSTEM_DRIVER  = 0x00000002;

    public static final int TYPE_ADAPTER             = 0x00000004;

    public static final int TYPE_RECOGNIZER_DRIVER   = 0x00000008;
    /**
     * Service that runs in its own process.
     */
    public static final int TYPE_WIN32_OWN_PROCESS   = 0x00000010;
    /**
     * Service that shares a process with other services.
     */
    public static final int TYPE_WIN32_SHARE_PROCESS = 0x00000020;
    /**
     * The service can interact with the desktop.
     */
    public static final int TYPE_INTERACTIVE_PROCESS = 0x00000100;

    // Error control type
    /**
     * The startup (boot) program logs the error but continues the startup operation.
     */
    public static final int ERROR_IGNORE   = 0x00000000;
    /**
     * The startup program logs the error and displays a message box pop-up but continues the startup operation.
     */
    public static final int ERROR_NORMAL   = 0x00000001;
    /**
     * The startup program logs the error.
     * If the last-known good configuration is being started, the startup operation continues.
     * Otherwise, the system is restarted with the last-known-good configuration.
     */
    public static final int ERROR_SEVERE   = 0x00000002;
    /**
     * The startup program logs the error, if possible.
     * If the last-known good configuration is being started, the startup operation fails.
     * Otherwise, the system is restarted with the last-known good configuration.
     */
    public static final int ERROR_CRITICAL = 0x00000003;

    int type;
    int startType;
    int errorControl;
    String binaryPathName;
    String loadOrderGroup;
    int tagId;
    String[] dependencies = null;
    String serviceStartName;
    String displayName;
    String description;

    /**
     * @return Returns the binaryPathName.
     */
    public String getBinaryPathName() {
        return binaryPathName;
    }
    /**
     * @param binaryPathName The binaryPathName to set.
     */
    public void setBinaryPathName(String binaryPathName) {
        this.binaryPathName = binaryPathName;
    }
    /**
     * @return Returns the dependencies.
     */
    public String[] getDependencies() {
        if (this.dependencies == null) {
            return new String[0];
        }
        return dependencies;
    }
    /**
     * @param dependencies The dependencies to set.
     */
    public void setDependencies(String[] dependencies) {
        this.dependencies = dependencies;
    }
    /**
     * @return Returns the displayName.
     */
    public String getDisplayName() {
        return displayName;
    }
    /**
     * @param displayName The displayName to set.
     */
    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }
    /**
     * @return Returns the errorControl, one of ERROR_* constants.
     */
    public int getErrorControl() {
        return errorControl;
    }
    /**
     * @param errorControl The errorControl to set, one of ERROR_* constants.
     */
    public void setErrorControl(int errorControl) {
        this.errorControl = errorControl;
    }
    /**
     * @return Returns the loadOrderGroup.
     */
    public String getLoadOrderGroup() {
        return loadOrderGroup;
    }
    /**
     * @param loadOrderGroup The loadOrderGroup to set.
     */
    public void setLoadOrderGroup(String loadOrderGroup) {
        this.loadOrderGroup = loadOrderGroup;
    }
    /**
     * @return Returns the serviceStartName.
     */
    public String getServiceStartName() {
        return serviceStartName;
    }
    /**
     * @param serviceStartName The serviceStartName to set.
     */
    public void setServiceStartName(String serviceStartName) {
        this.serviceStartName = serviceStartName;
    }
    /**
     * @return Returns the startType, one of START_* constants.
     */
    public int getStartType() {
        return startType;
    }
    /**
     * @param startType The startType to set, one of START_* constants.
     */
    public void setStartType(int startType) {
        this.startType = startType;
    }
    /**
     * @return Returns the tagId.
     */
    public int getTagId() {
        return tagId;
    }
    /**
     * @param tagId The tagId to set.
     */
    public void setTagId(int tagId) {
        this.tagId = tagId;
    }
    /**
     * @return Returns the type, one of TYPE_* constants.
     */
    public int getType() {
        return type;
    }
    /**
     * @param type The type to set, one of TYPE_* constants.
     */
    public void setType(int type) {
        this.type = type;
    }
    /**
     * @return Returns the description.
     */
    public String getDescription() {
        return description;
    }
    /**
     * @param description The description to set.
     */
    public void setDescription(String description) {
        this.description = description;
    }
}
