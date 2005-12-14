package net.hyperic.sigar.cmd;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.DirStat;
import net.hyperic.sigar.FileInfo;
import net.hyperic.sigar.FileWatcher;
import net.hyperic.sigar.FileWatcherThread;

/**
 * Watch a file or directory displaying attribute changes.
 */
public class Watch {

    private static void printHeader(Sigar sigar, FileInfo info)
        throws SigarException {

        String file = info.getName();
        FileInfo link = sigar.getLinkInfo(file);

        if (link.getType() == FileInfo.TYPE_LNK) {
            try {
                System.out.println(file + " -> " +
                                   new File(file).getCanonicalPath());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        System.out.println(link.getTypeChar() + 
                           info.getPermissionsString() + "\t" +
                           info.getUid() + "\t" + info.getGid() + "\t" +
                           info.getSize() + "\t" +
                           new Date(info.getMtime()) + "\t" +
                           file);

        if (info.getType() == FileInfo.TYPE_DIR) {
            info.enableDirStat(true);

            DirStat stats = sigar.getDirStat(file);
            System.out.println("   Files......." + stats.getFiles()); 
            System.out.println("   Subdirs....." + stats.getSubdirs()); 
            System.out.println("   Symlinks...." + stats.getSymlinks()); 
            System.out.println("   Chrdevs....." + stats.getChrdevs()); 
            System.out.println("   Blkdevs....." + stats.getBlkdevs()); 
            System.out.println("   Sockets....." + stats.getSockets()); 
            System.out.println("   Total......." + stats.getTotal());
            System.out.println("   Disk Usage.." + stats.getDiskUsage());
        }
    }

    private static void add(Sigar sigar,
                            FileWatcher watcher,
                            String file,
                            boolean recurse)
        throws SigarException {

        FileInfo info = watcher.add(file);
        printHeader(sigar, info);

        if (!recurse) {
            return;
        }

        if (info.getType() == FileInfo.TYPE_DIR) {
            File[] dirs = 
                new File(info.getName()).listFiles(new FileFilter() {
                    public boolean accept(File file) {
                        return file.isDirectory() && file.canRead();
                    }
                });

            for (int i=0; i<dirs.length; i++) {
                add(sigar, watcher, dirs[i].getAbsolutePath(), recurse);
            }
        }
    }

    public static void main(String[] args) throws SigarException {
        boolean recurse = false;
        Sigar sigar = new Sigar();

        FileWatcherThread watcherThread = 
            FileWatcherThread.getInstance();

        watcherThread.setInterval(1000);

        FileWatcher watcher =
            new FileWatcher(sigar) {
                public void onChange(FileInfo info) {
                    System.out.println(info.getName() +
                                       " Changed:\n" +
                                       info.diff());
                }

                public void onNotFound(FileInfo info) {
                    System.out.println(info.getName() + " no longer exists");
                    remove(info.getName());
                }

                public void onException(FileInfo info, SigarException e) {
                    System.out.println("Error checking " + info.getName() + ":");
                    e.printStackTrace();
                }
            };

        watcher.setInterval(watcherThread.getInterval());
        for (int i=0; i<args.length; i++) {
            if (args[i].equals("-r")) {
                recurse = true;
                continue;
            }
            add(sigar, watcher, args[i], recurse);
        }

        watcherThread.add(watcher);

        watcherThread.doStart();

        System.out.println("Press any key to stop");
        try {
            System.in.read();
        } catch (IOException e) { }

        watcherThread.doStop();
    }
}
