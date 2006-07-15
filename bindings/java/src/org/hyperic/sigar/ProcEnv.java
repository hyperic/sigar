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

import java.util.Map;

/**
 * Lookup environment for a process.
 */
class ProcEnv {

    private ProcEnv () { }

    /**
     * @param sigar The Sigar object.
     * @param pid Process id.
     * @return Map of environment.
     * @exception SigarException on failure.
     * @see org.hyperic.sigar.Sigar#getProcEnv
     */
    public static native Map getAll(Sigar sigar, long pid)
        throws SigarException;

    /**
     * @param sigar The Sigar object.
     * @param pid Process id.
     * @param key Environment variable name.
     * @return Environment variable value.
     * @exception SigarException on failure.
     * @see org.hyperic.sigar.Sigar#getProcEnv
     */
    public static native String getValue(Sigar sigar, long pid, String key)
        throws SigarException;
}
