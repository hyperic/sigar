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

package org.hyperic.sigar.cmd;

import org.hyperic.sigar.SigarException;

public class Arp extends SigarCommandBase {
    public Arp(Shell shell) {
        super(shell);
    }

    public Arp() {
        super();
    }

    public String getUsageShort() {
        return "Display system ARP cache";
    }

    //arp -an
    public void output(String[] args) throws SigarException {
        org.hyperic.sigar.Arp[] entries = this.sigar.getArpList();

        for (int i=0; i<entries.length; i++) {
            org.hyperic.sigar.Arp arp = entries[i];
            String host = "?"; //XXX
            println(host + " " +
                    "(" + arp.getAddress() + ")" + " at " +
                    arp.getHwaddr() + " " +
                    "[" + arp.getType() + "]" + " on " +
                    arp.getIfname());
        }
    }

    public static void main(String[] args) throws Exception {
        new Arp().processCommand(args);
    }
}
