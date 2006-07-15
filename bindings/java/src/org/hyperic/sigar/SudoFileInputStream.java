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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

/**
 * InputStream wrapper for 'sudo cat filename'
 */
public class SudoFileInputStream extends InputStream {
    private Process process;
    private InputStream is;

    public SudoFileInputStream(String file)
        throws IOException, FileNotFoundException {
        this(new File(file));
    }

    public SudoFileInputStream(File file)
        throws IOException, FileNotFoundException {
        if (!file.exists()) {
            throw new FileNotFoundException();
        }
        String[] args = {
            "sudo", "cat", file.toString()
        };
        this.process = Runtime.getRuntime().exec(args);
        this.is = this.process.getInputStream();
    }

    public void close() throws IOException {
        this.process.destroy();
    }

    /* passthru all other InputStream methods */
    public int read() throws IOException {
        return this.is.read();
    }

    public int read(byte b[]) throws IOException {
        return this.is.read(b);
    }

    public int read(byte b[], int off, int len) throws IOException {
        return this.is.read(b, off, len);
    }

    public long skip(long n) throws IOException {
        return this.is.skip(n);
    }

    public int available() throws IOException {
        return this.is.available();
    }

    public synchronized void mark(int readlimit) {
        this.is.mark(readlimit);
    }

    public synchronized void reset() throws IOException {
        this.is.reset();
    }

    public boolean markSupported() {
        return this.is.markSupported();
    }
}
