/*
 * Copyright (C) [2004-2009], Hyperic, Inc.
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
import java.util.Map;

/**
 * Object wrapper of double[] Sigar.getLoadAverage() for use in bean patterns (JMX). 
 */
public class LoadAverage {

    private double[] average;

    public LoadAverage(double[] average) {
        this.average = average;
    }

    public double getOneMinute() {
        return this.average[0];
    }

    public double getFiveMinute() {
        return this.average[1];
    }

    public double getFifteenMinute() {
        return this.average[2];
    }

    public Map toMap() {
        Map map = new HashMap();
        map.put("OneMinute", new Double(getOneMinute()));
        map.put("FiveMinute", new Double(getFiveMinute()));
        map.put("FifteenMinute", new Double(getFifteenMinute()));
        return map;
    }
}
