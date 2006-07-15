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

package org.hyperic.sigar;

import java.util.HashMap;

import java.io.IOException;
import java.io.File;

/**
 * Helper class to build a map of mounted file systems.
 */
public class FileSystemMap extends HashMap {

    /**
     * FileSystemMap is read-only, this method is unsupported.
     * @see #init
     */
    public Object put(Object key, Object value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Populate the map.  FileSystem.getDirName is used as the map key.
     */
    public void init(FileSystem[] fslist) {
        super.clear();

        for (int i=0; i<fslist.length; i++) {
            super.put(fslist[i].getDirName(), fslist[i]);
        }
    }

    public FileSystem getFileSystem(String name) {
        return (FileSystem)get(name);
    }

    public boolean isMounted(String name) {
        return (get(name) != null);
    }

    /**
     * Find the file system the given file or directory is within.
     * @return FileSystem or null if file or directory name does not exist.
     */
    public FileSystem getMountPoint(String name) {
        FileSystem fs = getFileSystem(name);
        if (fs != null) {
            return fs;
        }

        File dir = new File(name);
        if (!dir.exists()) {
            return null;
        }

        try {
            dir = dir.getCanonicalFile();
        } catch (IOException e) {
            throw new IllegalArgumentException(e.getMessage());
        }

        if (!dir.isDirectory()) {
            dir = dir.getParentFile();
        }

        do {
            fs = getFileSystem(dir.toString());
            if (fs != null) {
                return fs;
            }
            dir = dir.getParentFile();
        } while (dir != null);

        return null;
    }
}
