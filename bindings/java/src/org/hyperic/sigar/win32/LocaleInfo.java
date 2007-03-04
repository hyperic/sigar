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

public class LocaleInfo extends Win32 {

    /**
     * English name of language
     */
    public static final int LOCALE_SENGLANGUAGE = 0x00001001;

    /**
     * English name of country
     */
    public static final int LOCALE_SENGCOUNTRY = 0x00001002;

    /**
     * English primary language id
     */
    public static final int LANG_ENGLISH = 0x09;

    private int id;

    private static native int getSystemDefaultLCID();

    private static native String getAttribute(int id, int attr);

    public LocaleInfo() {
        this(getSystemDefaultLCID());
    }

    public static final int makeLangId(int primary, int sub) {
        return (sub << 10) | primary;
    }

    public LocaleInfo(Integer id) {
        this(id.intValue());
    }

    public LocaleInfo(int id) {
        this.id = id;
    }

    public LocaleInfo(int primary, int sub) {
        this(makeLangId(primary, sub));
    }

    public int getId() {
        return this.id;
    }

    public void setId(int id) {
        this.id = id;
    }

    private static int getPrimaryLangId(int id) {
        return id & 0x3ff;
    }

    public int getPrimaryLangId() {
        return getPrimaryLangId(this.id);
    }

    private static int getSubLangId(int id) {
        return id >> 10;
    }

    public int getSubLangId() {
        return getSubLangId(this.id);
    }

    public static boolean isEnglish() {
        int id = getSystemDefaultLCID();
        return getPrimaryLangId(id) == LANG_ENGLISH;
    }

    public String getPerflibLangId() {
        String id =
            Integer.toHexString(getPrimaryLangId()).toUpperCase();

        //length always == 3
        int pad = 3 - id.length();
        StringBuffer fid = new StringBuffer(3);
        while (pad-- > 0) {
            fid.append("0");
        }
        fid.append(id);

        return fid.toString();
    }

    public String getAttribute(int attr) {
        return getAttribute(this.id, attr);
    }

    public String getEnglishLanguageName() {
        return getAttribute(LOCALE_SENGLANGUAGE);
    }

    public String getEnglishCountryName() {
        return getAttribute(LOCALE_SENGCOUNTRY);
    }

    public String toString() {
        return
            getId() + ":" +
            getEnglishLanguageName() +
            " (" + getEnglishCountryName() + ")";
    }
}
