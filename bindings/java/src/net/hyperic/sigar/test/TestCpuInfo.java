package net.hyperic.sigar.test;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.CpuInfo;

public class TestCpuInfo extends SigarTestCase {

    public TestCpuInfo(String name) {
        super(name);
    }

    public void testCreate() throws Exception {
        Sigar sigar = getSigar();

        CpuInfo[] infos = sigar.getCpuInfoList();

        for (int i=0; i<infos.length; i++) {
            CpuInfo info = infos[i];

            traceln("num=" + i);
            traceln("vendor=" + info.getVendor());
            traceln("model=" + info.getModel());
            traceln("mhz=" + info.getMhz());
            traceln("cache size=" + info.getCacheSize());
        }

        int mhz = infos[0].getMhz();
        int current = sigar.getCpuInfoList()[0].getMhz();
        assertEquals("Mhz=" + current + "/" + mhz, current, mhz);
    }
}
