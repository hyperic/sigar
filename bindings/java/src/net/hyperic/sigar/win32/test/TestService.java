package net.hyperic.sigar.win32.test;

import net.hyperic.sigar.test.SigarTestCase;

import net.hyperic.sigar.win32.Service;
import net.hyperic.sigar.win32.ServiceConfig;

public class TestService extends SigarTestCase {
    private static final String TEST_NAME = "MyTestService";

    private static final String PREFIX =
        "sigar.test.service.";
    
    private static final boolean TEST_CREATE =
        "true".equals(System.getProperty(PREFIX + "create"));

    private static final boolean TEST_DELETE =
        "true".equals(System.getProperty(PREFIX + "delete"));
    
    public TestService(String name) {
        super(name);
    }

    public void testServiceOpen() throws Exception {
        Service service = new Service("Eventlog");
        service.getConfig();
        service.close();
    }

    public void testServiceCreateDelete() throws Exception {
        if (!TEST_CREATE) {
            return;
        }
        ServiceConfig config = new ServiceConfig(TEST_NAME);
        config.setStartType(ServiceConfig.START_MANUAL);
        config.setDisplayName("My Test Service");
        config.setDescription("A Description of " + config.getDisplayName());
        config.setPath("C:\\Program Files\\My Test 1.0\\mytest.exe");

        Service.create(config);
    }

    public void testDeleteService() throws Exception {
        if (!TEST_DELETE) {
            return;
        }
        Service service = new Service(TEST_NAME);
        service.delete();
    }
}
