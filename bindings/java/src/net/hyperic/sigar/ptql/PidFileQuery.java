package net.hyperic.sigar.ptql;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import net.hyperic.sigar.SigarException;

public class PidFileQuery extends PidQuery {

    File file;
    long modified = -1;

    public PidFileQuery(String file) {
        this.file = new File(file);
    }

    public long getPid()
        throws SigarException {

        if (!file.exists()) {
            throw new SigarException(this.file + " does not exist");
        }

        long lastMod = file.lastModified();
        if (lastMod == this.modified) {
            return this.pid;
        }

        this.modified = lastMod;

        String line;

        try {
            BufferedReader in =
                new BufferedReader(new FileReader(this.file));
            line = in.readLine();
        } catch (FileNotFoundException e) {
            throw new SigarException(e.getMessage());
        } catch (IOException e) {
            throw new SigarException(e.getMessage());
        }

        try {
            this.pid = Long.parseLong(line);
        } catch (NumberFormatException e) {
            throw new SigarException("Not a number: " + line);
        }

        return this.pid;
    }
}
