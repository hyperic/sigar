package net.hyperic.sigar.win32;

import java.util.Arrays;
import java.util.List;

public class Service extends Win32 {
    // Service State
    public static final int SERVICE_STOPPED          = 0x00000001;
    public static final int SERVICE_START_PENDING    = 0x00000002;
    public static final int SERVICE_STOP_PENDING     = 0x00000003;
    public static final int SERVICE_RUNNING          = 0x00000004;
    public static final int SERVICE_CONTINUE_PENDING = 0x00000005;
    public static final int SERVICE_PAUSE_PENDING    = 0x00000006;
    public static final int SERVICE_PAUSED           = 0x00000007;

    private static final String[] STATUS = {
        "Unknown",
        "Stopped",
        "Start Pending",
        "Stop Pending",
        "Running",
        "Continue Pending",
        "Pause Pending",
        "Paused"
    };
    
    // Service Controls
    private static final int SERVICE_CONTROL_STOP             = 0x00000001;
    private static final int SERVICE_CONTROL_PAUSE            = 0x00000002;
    private static final int SERVICE_CONTROL_CONTINUE         = 0x00000003;
    private static final int SERVICE_CONTROL_INTERROGATE      = 0x00000004;
    private static final int SERVICE_CONTROL_SHUTDOWN         = 0x00000005;
    private static final int SERVICE_CONTROL_PARAMCHANGE      = 0x00000006;
    private static final int SERVICE_CONTROL_NETBINDADD       = 0x00000007;
    private static final int SERVICE_CONTROL_NETBINDREMOVE    = 0x00000008;
    private static final int SERVICE_CONTROL_NETBINDENABLE    = 0x00000009;
    private static final int SERVICE_CONTROL_NETBINDDISABLE   = 0x0000000A;
    private static final int SERVICE_CONTROL_DEVICEEVENT      = 0x0000000B;
    private static final int SERVICE_CONTROL_HARDWAREPROFILECHANGE 
        = 0x0000000C;
    private static final int SERVICE_CONTROL_POWEREVENT       = 0x0000000D;
    private static final int SERVICE_CONTROL_SESSIONCHANGE    = 0x0000000E;

    // Service Control Manager object specific access types
    private static final int STANDARD_RIGHTS_REQUIRED = (int)0x000F0000L;

    private static final int SC_MANAGER_CONNECT            = 0x0001;
    private static final int SC_MANAGER_CREATE_SERVICE     = 0x0002;
    private static final int SC_MANAGER_ENUMERATE_SERVICE  = 0x0004;
    private static final int SC_MANAGER_LOCK               = 0x0008;
    private static final int SC_MANAGER_QUERY_LOCK_STATUS  = 0x0010;
    private static final int SC_MANAGER_MODIFY_BOOT_CONFIG = 0x0020;

    private static final int SC_MANAGER_ALL_ACCESS = 
        (STANDARD_RIGHTS_REQUIRED |
         SC_MANAGER_CONNECT             |
         SC_MANAGER_CREATE_SERVICE      |
         SC_MANAGER_ENUMERATE_SERVICE   |
         SC_MANAGER_LOCK                |
         SC_MANAGER_QUERY_LOCK_STATUS   |
         SC_MANAGER_MODIFY_BOOT_CONFIG);

    // Service object specific access type
    private static final int SERVICE_QUERY_CONFIG         = 0x0001;
    private static final int SERVICE_CHANGE_CONFIG        = 0x0002;
    private static final int SERVICE_QUERY_STATUS         = 0x0004;
    private static final int SERVICE_ENUMERATE_DEPENDENTS = 0x0008;
    private static final int SERVICE_START                = 0x0010;
    private static final int SERVICE_STOP                 = 0x0020;
    private static final int SERVICE_PAUSE_CONTINUE       = 0x0040;
    private static final int SERVICE_INTERROGATE          = 0x0080;
    private static final int SERVICE_USER_DEFINED_CONTROL = 0x0100;

    private static final int SERVICE_ALL_ACCESS =
        (STANDARD_RIGHTS_REQUIRED |
         SERVICE_QUERY_CONFIG           |
         SERVICE_CHANGE_CONFIG          |
         SERVICE_QUERY_STATUS           |
         SERVICE_ENUMERATE_DEPENDENTS   |
         SERVICE_START                  |
         SERVICE_STOP                   |
         SERVICE_PAUSE_CONTINUE         |
         SERVICE_INTERROGATE            |
         SERVICE_USER_DEFINED_CONTROL);
    
    private long manager;
    private long service;
    private String name;

    private Service() throws Win32Exception
    {
        this.manager = OpenSCManager("", SC_MANAGER_ALL_ACCESS);
        
        if (this.manager == 0) {
            throw getLastErrorException();
        }
    }

    public static native List getServiceNames() throws Win32Exception;

    public Service(String serviceName) throws Win32Exception
    {
        this();
        
        this.service = OpenService(this.manager, serviceName, 
                                   SERVICE_ALL_ACCESS);

        if (this.service == 0) {
            throw getLastErrorException();
        }
        
        this.name = serviceName;
    }

    protected void finalize()
    {
        close();
    }

    public synchronized void close()
    {
        if (this.service != 0) {
            CloseServiceHandle(this.service);
            this.service = 0;
        }

        if (this.manager != 0) {
            CloseServiceHandle(this.manager);
            this.manager = 0;
        }
    }
    
    public static Service create(ServiceConfig config)
        throws Win32Exception
    {
        if (config.getName() == null) {
            throw new IllegalArgumentException("name=null");
        }

        if (config.getPath() == null) {
            throw new IllegalArgumentException("path=null");
        }

        Service service = new Service();
        service.service =
            CreateService(service.manager,
                          config.getName(),
                          config.getDisplayName(),
                          config.getType(),
                          config.getStartType(),
                          config.getErrorControl(),
                          config.getPath(),
                          config.getDependencies(),
                          config.getServiceStartName(),
                          config.getPassword());

        if (service.service == 0) {
            throw getLastErrorException();
        }

        if (config.getDescription() != null) {
            service.setDescription(config.getDescription());
        }
        
        return service;
    }

    public void delete() throws Win32Exception
    {
        DeleteService(this.service);
    }

    public void control(int control)
        throws Win32Exception
    {
        if (!ControlService(this.service, control)) {
            throw getLastErrorException();
        }
    }

    public void setDescription(String description)
    {
        ChangeServiceDescription(this.service, description);
    }
            
    public void start() throws Win32Exception
    {
        if (StartService(this.service) == false) {
            throw getLastErrorException();
        }
    }

    public void startAndWait() throws Win32Exception
    {
        // Wait indefinitely
        startAndWait(0);
    }

    public void startAndWait(long timeout) throws Win32Exception
    {
        start();
        waitForStart(timeout);
    }

    public int status()
    {
        return QueryServiceStatus(this.service);
    }

    public String getStatusString()
    {
        return STATUS[status()];
    }
    
    public void stop() throws Win32Exception
    {
        if (StopService(this.service) == false) {
            throw getLastErrorException();
        }
    }

    public void stopAndWait() throws Win32Exception
    {
        // Wait indefinitely
        stopAndWait(0);
    }

    public void stopAndWait(long timeout) throws Win32Exception
    {
        long status;
        
        stop();

        long start = System.currentTimeMillis();

        while ((status = status()) != SERVICE_STOPPED) {
            if (status == SERVICE_STOP_PENDING) {
                // The start hasn't completed yet. Keep trying up to 
                // the timeout.
                if (((System.currentTimeMillis() - start) < timeout) || 
                    (timeout <= 0))
                {
                    try {
                        Thread.sleep(100);
                    } catch(InterruptedException e) {
                    }
                }
            }
            else {
                break;
            }
        }

        if (status != SERVICE_STOPPED) {
            throw getLastErrorException();
        }
    }
    
    public void waitForStart(long timeout) throws Win32Exception
    {
        long    status;
        boolean result = true;
        long    start  = System.currentTimeMillis();
            
        while ((status = status()) != SERVICE_RUNNING) {
            if (status == SERVICE_START_PENDING) {
                // The start hasn't completed yet. Keep trying up to 
                // the timeout.
                if (((System.currentTimeMillis() - start) < timeout) ||
                    (timeout <= 0))
                {
                    try {
                        Thread.sleep(100);
                    } catch(InterruptedException e) {
                    }
                }
                else {
                    result = false;
                }
            } else if (status == SERVICE_STOPPED) {
                // Start failed
                result = false;
                break;
            } else {
                // Hrm.
                result = false;
                break;
            }
        }

        if (result == false) {
            throw getLastErrorException();
        }
    }

    public ServiceConfig getConfig() throws Win32Exception {
        ServiceConfig config = new ServiceConfig();
        if (!QueryServiceConfig(this.service, config)) {
            throw getLastErrorException();
        }
        config.setName(this.name);
        return config;
    }
    
    private static Win32Exception getLastErrorException() 
    {
        int err = GetLastError();
        return new Win32Exception(err, "Win32 Error Code: " + 
                                  err + ": " + GetErrorMessage(err));
    }
    
    private static native boolean ChangeServiceDescription(long handle,
                                                           String description);

    private static native boolean CloseServiceHandle(long handle);

    private static native long CreateService(long handle,
                                             String serviceName,
                                             String displayName,
                                             int serviceType,
                                             int startType,
                                             int errorControl,
                                             String path,
                                             String[] dependencies,
                                             String startName,
                                             String password);

    private static native boolean ControlService(long handle,
                                                 int control);

    private static native boolean DeleteService(long handle);

    private static native long OpenSCManager(String machine,
                                             int access);

    private static native long OpenService(long handle,
                                           String service,
                                           int access);

    private static native int QueryServiceStatus(long handle);

    private static native boolean StartService(long handle);

    private static native boolean StopService(long handle);

    private static native boolean QueryServiceConfig(long handle,
                                                     ServiceConfig config);

    public static void main(String[] args) throws Exception {
        List services;
        if (args.length == 0) {
            services = getServiceNames();
        }
        else {
            services = Arrays.asList(args);
        }

        for (int i=0; i<services.size(); i++) {
            String name = (String)services.get(i);
            Service service = new Service(name);
            service.getConfig().list(System.out);
            System.out.println("status........[" +
                               service.getStatusString() + "]");
            System.out.println("");
        }
    }
}
