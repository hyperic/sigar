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

package org.hyperic.sigar.win32.test;

import org.hyperic.sigar.test.SigarTestCase;

import org.hyperic.sigar.win32.Service;
import org.hyperic.sigar.win32.ServiceConfig;
import org.hyperic.sigar.win32.Win32Exception;

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
        
        String dummyName = "DOESNOTEXIST"; 
        try {
            new Service(dummyName);
            assertTrue(false);
        } catch (Win32Exception e) {
            traceln(dummyName + ": " + e.getMessage());
            assertTrue(true);
        }
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
