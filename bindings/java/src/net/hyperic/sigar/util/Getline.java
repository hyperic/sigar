package net.hyperic.sigar.util;

import java.io.IOException;
import java.io.EOFException;
import java.io.File;

public class Getline {

    private String prompt = "> ";

    public Getline() { }

    public Getline(String prompt) {
        this.prompt = prompt;
    }

    public native static void setCompleter(GetlineCompleter completer);

    public native void redraw();

    public native void reset();

    private native void histadd(String line);

    private native void histinit(String file);

    private native String getline(String prompt)
        throws IOException, EOFException;

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

        //XXX provide pure-java fallback
        String line = getline(prompt);
        if (addToHistory) {
            histadd(line);
        }
        return line;
    }

    public void initHistoryFile(File file)
        throws IOException {

        histinit(file.getCanonicalPath());
    }

    public void addToHistory(String line) {
        histadd(line);
    }
}
