package net.hyperic.sigar.cmd;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.Reader;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.FileInfo;
import net.hyperic.sigar.FileTail;
import net.hyperic.sigar.FileWatcherThread;

public class Tail {

    public static void main(String[] args) throws SigarException {
        final String pattern;

        Sigar sigar = new Sigar();

        FileWatcherThread watcherThread = 
            FileWatcherThread.getInstance();

        watcherThread.doStart();

        watcherThread.setInterval(1000);

        FileTail watcher =
            new FileTail(sigar) {
                public void tail(FileInfo info, Reader reader) {
                    String line;
                    BufferedReader buffer =
                        new BufferedReader(reader);

                    if (getFiles().size() > 1) {
                        System.out.println("==> " +
                                           info.getName() +
                                           " <==");
                    }

                    try {
                        while ((line = buffer.readLine()) != null) {
                            System.out.println(line);
                        }
                    } catch (IOException e) {
                        System.out.println(e);                    
                    }
                }
            };

        for (int i=0; i<args.length; i++) {
            watcher.add(args[i]);
        }

        watcherThread.add(watcher);

        try {
            System.in.read();
        } catch (IOException e) { }

        watcherThread.doStop();
    }
}
