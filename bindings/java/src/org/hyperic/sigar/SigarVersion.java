/*
 * Copyright (c) 2009 Hyperic, Inc.
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
