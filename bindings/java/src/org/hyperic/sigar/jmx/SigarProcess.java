/*
 * Copyright (c) 2006-2009 Hyperic, Inc.
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

package org.hyperic.sigar.jmx;

import org.hyperic.sigar.ProcCpu;
import org.hyperic.sigar.ProcFd;
import org.hyperic.sigar.ProcMem;
import org.hyperic.sigar.ProcUtil;
import org.hyperic.sigar.ProcDiskIO;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;
import org.hyperic.sigar.SigarProxyCache;

/**
 * Implement the SigarProcessMBean to provide current process info
 * via JMX.
 */

public class SigarProcess implements SigarProcessMBean {

    private static final Long NOTIMPL = new Long(Sigar.FIELD_NOTIMPL);
    private Sigar sigarImpl;
    private SigarProxy sigar;
    private long pid = -1;

    public SigarProcess() {
        this.sigarImpl = new Sigar();
        this.sigar = SigarProxyCache.newInstance(sigarImpl);
    }

    public SigarProcess(SigarProxy sigar) {
        this.sigar = sigar;
    }

    public void close() {
        if (this.sigarImpl != null) {
            this.sigarImpl.close();
        }
    }

    private RuntimeException unexpectedError(String type,
                                             SigarException e) {
        String msg =
            "Unexected error in Sigar.get" + type +
            ": " + e.getMessage();
        return new IllegalArgumentException(msg);
    }
                                   
    private synchronized ProcMem getMem() {
        try {
            return this.sigar.getProcMem(getPid());
        } catch (SigarException e) {
            throw unexpectedError("Mem", e);
        }
    }

    private synchronized ProcCpu getCpu() {
        try {
            return this.sigar.getProcCpu(getPid());
        } catch (SigarException e) {
            throw unexpectedError("Cpu", e);
        }   
    }


    private synchronized ProcDiskIO getDiskIO() {
	try {
            return this.sigar.getProcDiskIO(getPid());
        } catch (SigarException e) {
            throw unexpectedError("DiskIO", e);
        }
    }


    private synchronized ProcFd getFd() throws SigarException {
        return this.sigar.getProcFd(getPid());
    }

    public String getObjectName() throws SigarException {
        long pid = getPid();
        String name = this.sigar.getProcState(pid).getName().replaceAll(":", "_");
        String cls = "unknown";
        if (name.startsWith("java")) {
            try {
                cls = ProcUtil.getJavaMainClass(this.sigar, pid);
            } catch (SigarException e) {}
        } //else XXX
        return
            AbstractMBean.MBEAN_DOMAIN + ":" +
            AbstractMBean.MBEAN_ATTR_TYPE + "=" + "Process" + "," +
            "Name" + "=" + name + "," +
            "Class" + "=" + cls + "," +
            "Pid" + "=" + pid;
    }

    public long getPid() {
        if (this.pid < 0) {
            return this.sigar.getPid();
        }
        else {
            return this.pid;
        }
    }

    public void setPid(long pid) {
        this.pid = pid;
    }

    public Long getMemSize() {
        return new Long(getMem().getSize());
    }

    /**
     * @deprecated
     * @see getMemSize
     */
    public Long getMemVsize() {
        return getMemSize();
    }

    public Long getMemResident() {
        return new Long(getMem().getResident());
    }

    public Long getMemShare() {
        return new Long(getMem().getShare());
    }

    public Long getMemPageFaults() {
        return new Long(getMem().getPageFaults());
    }

    public Long getTimeUser() {
        return new Long(getCpu().getUser());
    }

    public Long getTimeSys() {
        return new Long(getCpu().getSys());
    }

    public Double getCpuUsage() {
        return new Double(getCpu().getPercent());
    }

    public Long getOpenFd() {
        try {
            return new Long(getFd().getTotal());
        } catch (SigarException e) {
            return NOTIMPL;
        }
    }

     public Double getBytesReadWriteTotal() {
        return new Double(getDiskIO().getBytesTotal());
    }

}
