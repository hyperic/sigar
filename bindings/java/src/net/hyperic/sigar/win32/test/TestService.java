package net.hyperic.sigar.win32.test;

import junit.framework.TestCase;
import net.hyperic.sigar.win32.Service;

public class TestService extends TestCase {

    public TestService(String name) {
        super(name);
    }

    public void testServiceCreateDelete() throws Exception {

        Service service =
            Service.create("MyTestService", 
                           "My Test Service",
                           "This is a great service.", 
                           "C:\\oracle\\ora90\\bin\\agntsrvc.exe");
    }

    public void testServiceOpen() throws Exception {
        Service service = new Service("MyTestService");
    }

    public void testDeleteService() throws Exception {
        Service service = new Service("MyTestService");
        service.delete();
    }
}
