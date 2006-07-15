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

package org.hyperic.sigar.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.EOFException;
import java.io.File;

/**
 * This class provides Emacs style key bindings, history and tab completion
 * for command shell applications.
 *
 * This class is a Java interface to the getline.c package:
 * Copyright (C) 1991, 1992 by Chris Thewalt (thewalt@ce.berkeley.edu)
 */
public class Getline {

    private static final boolean isatty = isatty();

    private static boolean useNative =
        ! "false".equals(System.getProperty("sigar.getline.native")) &&
        isatty;

    private BufferedReader in = null;

    private String prompt = "> ";

    public Getline() { }

    public Getline(String prompt) {
        this.prompt = prompt;
    }

    private native static boolean isatty();

    public native static void setCompleter(GetlineCompleter completer);

    public native void redraw();

    public native void reset();

    private native void histadd(String line);

    private native void histinit(String file);

    private native String getline(String prompt)
        throws IOException, EOFException;

    public static boolean isTTY() {
        return isatty;
    }

    public String getLine()
        throws IOException, EOFException {

        return getLine(this.prompt, true);
    }

    public String getLine(String prompt)
        throws IOException, EOFException {

        return getLine(prompt, true);
    }

    public String getLine(String prompt, boolean addToHistory)
        throws IOException, EOFException {

        if (useNative) {
            String line = getline(prompt);
            if (addToHistory) {
                addToHistory(line);
            }
            return line;
        }
        else {
            if (this.in == null) {
                this.in =
                    new BufferedReader(new InputStreamReader(System.in));
            }
            System.out.print(prompt);
            return this.in.readLine();
        }
    }

    public void initHistoryFile(File file)
        throws IOException {

        if (useNative) {
            histinit(file.getCanonicalPath());
        }
    }

    public void addToHistory(String line) {
        if ((line == null) ||
            (line.length() == 0))
        {
            return;
        }
        if (useNative) {
            histadd(line);
        }
    }
}
