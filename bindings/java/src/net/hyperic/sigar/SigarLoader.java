package net.hyperic.sigar;

import net.hyperic.jni.ArchLoader;
import net.hyperic.jni.ArchName;
import net.hyperic.jni.ArchNotSupportedException;

public class SigarLoader extends ArchLoader {

    public SigarLoader(Class loaderClass) {
        super(loaderClass);
    }

    //XXX same as super.getArchLibName()
    //but db2monitor.jar gets loaded first in jboss
    //results in NoSuchMethodError
    public String getArchLibName()
        throws ArchNotSupportedException {

        return getName() + "-" + ArchName.getName();
    }

    public String getDefaultLibName()
        throws ArchNotSupportedException {

        return getArchLibName(); //drop "java" prefix
    }

    //override these methods to ensure our ClassLoader
    //loads the native library.
    protected void systemLoadLibrary(String name) {
        System.loadLibrary(name);
    }

    protected void systemLoad(String name) {
        System.load(name);
    }
}
