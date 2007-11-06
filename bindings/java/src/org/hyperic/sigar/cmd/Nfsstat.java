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

package org.hyperic.sigar.cmd;

import org.hyperic.sigar.SigarException;

import java.lang.reflect.Method;

public class Nfsstat extends SigarCommandBase {
    public Nfsstat(Shell shell) {
        super(shell);
    }

    public Nfsstat() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getUsageShort() {
        return "Display nfs stats";
    }

    private String getValue(Object obj, String attr) {
        if (attr == "") return "";
        String name =
            "get" + Character.toUpperCase(attr.charAt(0)) +
            attr.substring(1);
        try {
            Method method =
                obj.getClass().getMethod(name, new Class[0]);
            return method.invoke(obj, new Object[0]).toString();
        } catch (Exception e) {
            return "EINVAL";
        }
    }

    private void printnfs(Object nfs, String[] names) {
        String[] values = new String[names.length];
        for (int i=0; i<names.length; i++) {
            values[i] = getValue(nfs, names[i]);
        }
        printf(names);
        printf(values);
    }

    private void outputNfsV2(String header, Object nfs) {
        println(header + ":");
        printnfs(nfs,
                 new String[] { "null", "getattr", "setattr", "root", "lookup", "readlink" });

        printnfs(nfs,
                 new String[] { "read", "writecache", "write", "create", "remove", "rename" });

        printnfs(nfs,
                 new String[] { "link", "symlink", "mkdir", "rmdir", "readdir", "fsstat" });
        println("");
        flush();
    }

    private void outputNfsV3(String header, Object nfs) {
        println(header + ":");
        printnfs(nfs,
                 new String[] { "null", "getattr", "setattr", "lookup", "access", "readlink" });

        printnfs(nfs,
                 new String[] { "read", "write", "create", "mkdir", "symlink", "mknod" });

        printnfs(nfs,
                 new String[] { "remove", "rmdir", "rename", "link", "readdir", "readdirplus" });

        printnfs(nfs,
                 new String[] { "fsstat", "fsinfo", "pathconf", "commit", "", "" });

        println("");
        flush();
    }

    public void output(String[] args) throws SigarException {
        try {
            outputNfsV2("Client nfs v2",
                        this.sigar.getNfsClientV2());
        } catch (SigarException e) {}

        try {
            outputNfsV2("Server nfs v2",
                        this.sigar.getNfsServerV2());
        } catch (SigarException e) {}

        try {
            outputNfsV3("Client nfs v3",
                        this.sigar.getNfsClientV3());
        } catch (SigarException e) {}

        try {
            outputNfsV3("Server nfs v3",
                        this.sigar.getNfsServerV3());
        } catch (SigarException e) {}
    }
}
