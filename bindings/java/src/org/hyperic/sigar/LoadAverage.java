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
