package net.hyperic.sigar.win32.test;

import junit.framework.TestCase;
import net.hyperic.sigar.win32.Service;

public class TestService extends TestCase {

    private static final boolean TEST_CREATE = false;
    
    public TestService(String name) {
        super(name);
    }

    public void testServiceOpen() throws Exception {
        Service service = new Service("Eventlog");
    }

    public void testServiceCreateDelete() throws Exception {
        if (!TEST_CREATE) {
            return;
        }
        Service service =
            Service.create("MyTestService", 
                           "My Test Service",
                           "This is a great service.", 
                           "C:\\oracle\\ora90\\bin\\agntsrvc.exe");
    }

    public void testDeleteService() throws Exception {
        if (!TEST_CREATE) {
            return;
        }
        Service service = new Service("MyTestService");
        service.delete();
    }
}
