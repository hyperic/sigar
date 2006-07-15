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

/**
 * Sigar exception class, thrown for methods which are not implemented
 * on a given platform.
 */
public class SigarNotImplementedException extends SigarException {

    private static final String msg = 
        "This method has not been implemented on this platform";

    public static final SigarNotImplementedException INSTANCE =
        new SigarNotImplementedException(msg);

    public SigarNotImplementedException () { super(); }

    public SigarNotImplementedException (String s) { super(s); }
}
