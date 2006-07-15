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

package org.hyperic.sigar.pager;

import java.io.Serializable;

public class SortAttribute implements Serializable {

    private SortAttribute () {}

    public static final int DEFAULT = 0;

    // Generic attributes
    public static final int NAME           = 1;
    public static final int CTIME          = 2;

    // Authz sort attributes - specifieds which column to store on
    // for example, for 'subject_name', sort on column #3 
    public static final int ROLE_NAME      = 1;
    public static final int RESGROUP_NAME  = 2;    
    public static final int RESTYPE_NAME   = 4;
    public static final int RESOURCE_NAME  = 5;
    public static final int OPERATION_NAME = 6;
    public static final int ROLE_MEMBER_CNT= 17;
    
    public static final int SUBJECT_NAME   = 3;
    public static final int FIRST_NAME     = 7;
    public static final int LAST_NAME      = 8;
    
    // Event sort attributes
    public static final int EVENT_LOG_CTIME   = 1;

    // Control sort attributes
    public static final int CONTROL_ACTION          = 9;
    public static final int CONTROL_STATUS          = 10;
    public static final int CONTROL_STARTED         = 11;
    public static final int CONTROL_ELAPSED         = 12;
    public static final int CONTROL_DATESCHEDULED   = 13;
    public static final int CONTROL_DESCRIPTION     = 14;
    public static final int CONTROL_NEXTFIRE        = 15;
    public static final int CONTROL_ENTITYNAME      = 16;
    
    public static final int OWNER_NAME     = 21;
  
    public static final int SERVICE_NAME   = 22;
    public static final int SERVICE_TYPE   = 23;
    

    public static final int RT_NAME        = 24;
    public static final int RT_LOW         = 25;
    public static final int RT_AVG         = 26;
    public static final int RT_PEAK        = 27;

}
