/*
 * Copyright (c) 2010 VMware, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hyperic.sigar.test;

import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;
import org.hyperic.sigar.Arp;

public class TestArp extends SigarTestCase {

    public TestArp(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        try {
            Arp[] entries = getSigar().getArpList();
            for (int i=0; i<entries.length; i++) {
                Arp arp = entries[i];
                assertTrueTrace("Address", arp.getAddress());
                assertTrueTrace("Ifname", arp.getIfname());
                assertTrueTrace("Hwaddr", arp.getHwaddr());
                assertTrueTrace("Type", arp.getType());
                traceln("Flags=" + arp.getFlags());
            }
        } catch (SigarNotImplementedException e) {
            return;
        } catch (SigarException e) {
            return;
        }
    }
}
