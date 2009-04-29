package org.hyperic.sigar;

public class SigarVersion {

    /**
     * @return Version number of the Java sigar.jar library
     */
    public String getJarVersion() {
        return Sigar.VERSION_STRING;
    }                                                

    /**
     * @return Version number of the native sigar library
     */
    public String getNativeVersion() {
        return Sigar.NATIVE_VERSION_STRING;
    }                                      

    /**
     * @return Build date of the Java sigar.jar library
     */
    public String getJarBuildDate() {
        return Sigar.BUILD_DATE;
    }                                                  

    /**
     * @return Build date of the native sigar library
     */
    public String getNativeBuildDate() {
        return Sigar.NATIVE_BUILD_DATE;
    }                                        

    /**
     * @return Source code revision of the Java sigar.jar library
     */
    public String getJarSourceRevision() {
        return Sigar.SCM_REVISION;
    }                                           

    /**
     * @return Source code revision of the native sigar library
     */
    public String getNativeSourceRevision() {
        return Sigar.NATIVE_SCM_REVISION;
    }                                 

    /**
     * @return Name of the loaded native sigar library file
     */
    public String getNativeLibraryName() {
        return SigarLoader.getNativeLibraryName();
    }
}
