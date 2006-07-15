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

package org.hyperic.sigar.shell;

import java.io.File;
import java.io.FilenameFilter;

import java.util.Iterator;

import org.hyperic.sigar.SigarLoader;

public class FileCompleter
    extends CollectionCompleter
    implements FilenameFilter {

    private static final String HOME =
        System.getProperty("user.home");

    private String name;

    public FileCompleter() {
        super();
    }

    public FileCompleter(ShellBase shell) {
        super(shell);
    }

    public static String expand(String name) {
        if (name.startsWith("~")) {
            return HOME + name.substring(1, name.length());
        }
        return name;
    }

    public boolean accept(File dir, String name) {
        if (name.equals(".") || name.equals("..")) {
            return false;
        }
        return name.startsWith(this.name);
    }

    public Iterator getIterator() {
        return null; //unused
    }

    private String appendSep(String name) {
        if (name.endsWith(File.separator)) {
            return name;
        }

        return name + File.separator;
    }

    //e.g. we don't want "~/." treated as a directory
    //but we do want "." to be.
    private boolean isDotFile(File file) {
        return
            file.getName().equals(".") &&
            (file.getParentFile() != null);
    }

    public String complete(String line) {
        String fileName = line;
        boolean isHome = false;

        if (line.length() == 0) {
            return appendSep(".");
        }
        else if (fileName.startsWith("~")) {
            isHome = true;
            fileName = expand(fileName);
        }

        File file = new File(fileName);
        File dir;
        if (file.exists() && !isDotFile(file)) {
            if (file.isDirectory()) {
                this.name = null;
                dir = file;
                if (!fileName.endsWith(File.separator)) {
                    return line + File.separator;
                }
            }
            else {
                return line;
            }
        }
        else {
            this.name = file.getName();
            dir = file.getParentFile();
            if (dir == null) {
                if (SigarLoader.IS_WIN32 &&
                    (line.length() == 1) &&
                    Character.isLetter(line.charAt(0)))
                {
                    //e.g. C:\
                    return line + ":\\";
                }

                return line;
            }
            if (!(dir.exists() && dir.isDirectory())) {
                return line;
            }
        }

        String[] list;
        if (this.name == null) {
            list = dir.list();
        }
        else {
            list = dir.list(this);
        }

        if (list.length == 1) {
            fileName = appendSep(dir.toString()) + list[0];

            if (new File(fileName).isDirectory()) {
                fileName = appendSep(fileName);
            }
            if (isHome) {
                return "~" + fileName.substring(HOME.length(),
                                                fileName.length());
            }

            return fileName;
        }

        String partial = displayPossible(list);
        if (partial != null) {
            return appendSep(dir.toString()) + partial;
        }
        return line;
    }

    public static void main(String[] args) throws Exception {
        String line = new FileCompleter().complete(args[0]);
        System.out.println("\nsigar> '" + line + "'");
    }
}
