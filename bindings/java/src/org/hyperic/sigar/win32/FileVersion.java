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

package org.hyperic.sigar.win32;

import java.util.LinkedHashMap;
import java.util.Map;

public class FileVersion {
    private int
        product_major, product_minor, product_build, product_revision;
    private int
        file_major, file_minor, file_build, file_revision;
    private Map string_file_info = new LinkedHashMap();

    native boolean gather(String name);

    FileVersion() {}

    public int getProductMajor() {
        return this.product_major;
    }

    public int getProductMinor() {
        return this.product_minor;
    }

    public int getProductBuild() {
        return this.product_build;
    }

    public int getProductRevision() {
        return this.product_revision;
    }

    public int getFileMajor() {
        return this.file_major;
    }

    public int getFileMinor() {
        return this.file_minor;
    }

    public int getFileBuild() {
        return this.file_build;
    }

    public int getFileRevision() {
        return this.file_revision;
    }

    public Map getInfo() {
        return this.string_file_info;
    }

    private String toVersion(int major, int minor,
                             int build, int revision) {
        return
            major + "." +
            minor + "." +
            build + "." +
            revision;
    }

    public String getProductVersion() {
        return toVersion(this.product_major,
                         this.product_minor,
                         this.product_build,
                         this.product_revision);
    }

    public String getFileVersion() {
        return toVersion(this.file_major,
                         this.file_minor,
                         this.file_build,
                         this.file_revision);
    }
}
