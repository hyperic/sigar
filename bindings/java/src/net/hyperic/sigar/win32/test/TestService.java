package net.hyperic.sigar.win32.test;

import junit.framework.TestCase;
import net.hyperic.sigar.win32.Service;
import net.hyperic.sigar.win32.ServiceConfig;

public class TestService extends TestCase {

    private static final boolean TEST_CREATE = false;
    
    public TestService(String name) {
        super(name);
    }

    public void testServiceOpen() throws Exception {
        Service service = new Service("Eventlog");
        service.close();
    }

    public void testServiceCreateDelete() throws Exception {
        if (!TEST_CREATE) {
            return;
        }
        ServiceConfig config = new ServiceConfig("MyTestService");
        config.setDisplayName("My Test Service");
        config.setDescription("A Description of " + config.getDisplayName());
        config.setPath("C:\\Program Files\\My Test 1.0\\mytest.exe");

        Service.create(config);
    }

    public void testDeleteService() throws Exception {
        if (!TEST_CREATE) {
            return;
        }
        Service service = new Service("MyTestService");
        service.delete();
    }
}
