/*
 * Copyright (c) 2006-2007 Hyperic, Inc.
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
import org.hyperic.sigar.SigarPermissionDeniedException;

/**
 * Display all pid cache information.
 */
public class PidCacheInfo extends SigarCommandBase {


    public PidCacheInfo(Shell shell) {
        super(shell);
    }

    public PidCacheInfo() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display cache info for CPU cache and for IO cache";
    }

    public boolean isPidCompleter() {
        return false;
    }

    public void output(String[] args) throws SigarException {
        sigar.dumpPidCache();
    }
}
