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
    private static final int CONTROL_START  = 0x00000000;
    private static final int CONTROL_STOP   = 0x00000001;
    private static final int CONTROL_PAUSE  = 0x00000002;
    private static final int CONTROL_RESUME = 0x00000003;

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
    }

    public static native List getServiceNames() throws Win32Exception;

    public Service(String serviceName) throws Win32Exception
    {
        this();
        
        this.service = OpenService(this.manager, serviceName, 
                                   SERVICE_ALL_ACCESS);

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

        if (config.getDescription() != null) {
            service.setDescription(config.getDescription());
        }
        
        return service;
    }

    public void delete() throws Win32Exception
    {
        DeleteService(this.service);
    }

    public void setDescription(String description)
    {
        ChangeServiceDescription(this.service, description);
    }
            
    /**
     * @deprecated
     * @see #getStatus()
     */    
    public int status()
    {
        return getStatus();
    }
    
    public int getStatus()
    {
        return QueryServiceStatus(this.service);
    }

    public String getStatusString()
    {
        return STATUS[getStatus()];
    }
    
    private void control(int ctl) throws Win32Exception {
        ControlService(this.service, ctl);
    }

    private static class Waiter {
        long start = System.currentTimeMillis();
        Service service;
        long timeout;
        int wantedState;
        int pendingState;
        
        Waiter(Service service, 
               long timeout,
               int wantedState,
               int pendingState)
        {
            this.service = service;
            this.timeout = timeout;
            this.wantedState = wantedState;
            this.pendingState = pendingState;
        }
        
        boolean sleep() {
            int status;
            while ((status = service.getStatus()) != this.wantedState) {
                if (status != this.pendingState) {
                    return false;
                }

                if ((timeout <= 0) ||
                    ((System.currentTimeMillis() - this.start) < this.timeout))
                {
                    try {
                        Thread.sleep(100);
                    } catch(InterruptedException e) {}
                }
                else {
                    break;
                }
            }

            return status == this.wantedState;
        }
    }
    
    public void stop() throws Win32Exception
    {
        control(CONTROL_STOP);
    }

    public void stop(long timeout) throws Win32Exception
    {
        long status;
        
        stop();

        Waiter waiter =
            new Waiter(this, timeout, SERVICE_STOPPED, SERVICE_STOP_PENDING);

        if (!waiter.sleep()) {
            throw new Win32Exception("Failed to stop service");
        }
    }
    
    public void start() throws Win32Exception
    {
        control(CONTROL_START);
    }

    public void start(long timeout) throws Win32Exception
    {
        long status;

        start();

        Waiter waiter =
            new Waiter(this, timeout, SERVICE_RUNNING, SERVICE_START_PENDING);

        if (!waiter.sleep()) {
            throw new Win32Exception("Failed to start service");
        }
    }

    public ServiceConfig getConfig() throws Win32Exception {
        ServiceConfig config = new ServiceConfig();
        QueryServiceConfig(this.service, config);
        config.setName(this.name);
        return config;
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
                                             String password) throws Win32Exception;

    private static native void ControlService(long handle,
                                              int control) throws Win32Exception;

    private static native boolean DeleteService(long handle);

    private static native long OpenSCManager(String machine,
                                             int access) throws Win32Exception;

    private static native long OpenService(long handle,
                                           String service,
                                           int access) throws Win32Exception;

    private static native int QueryServiceStatus(long handle);

    private static native boolean QueryServiceConfig(long handle,
                                                     ServiceConfig config) throws Win32Exception;

    public static void main(String[] args) throws Exception {
        List services;
        if (args.length == 0) {
            services = getServiceNames();
        }
        else if ((args.length == 2) && (args[0].equals("-toggle"))) {
            long timeout = 5 * 1000; //5 seconds
            Service service = new Service(args[1]);
            if (service.getStatus() == SERVICE_RUNNING) {
                System.out.println("Stopping service...");
                service.stop(timeout);
            }
            else {
                System.out.println("Starting service...");
                service.start(timeout);
            }
            System.out.println(service.getStatusString());
            return;
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
