package org.hyperic.sigar.cmd;

import java.util.ArrayList;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.FileSystem;
import org.hyperic.sigar.FileSystemMap;
import org.hyperic.sigar.FileSystemUsage;

import org.hyperic.sigar.shell.FileCompleter;
import org.hyperic.sigar.util.GetlineCompleter;

/**
 * Report filesytem disk space usage.
 */
public class Iostat extends SigarCommandBase {

    private static final String OUTPUT_FORMAT =
        "%-15s %-15s %10s %10s %7s %7s %5s";

    private static final String[] HEADER = new String[] {
        "Filesystem",
        "Mounted on",
        "Reads",
        "Writes",
        "R-bytes",
        "W-bytes",
        "Queue",
    };

    private GetlineCompleter completer;

    public Iostat(Shell shell) {
        super(shell);
        setOutputFormat(OUTPUT_FORMAT);
        this.completer = new FileCompleter(shell);
    }

    public Iostat() {
        super();
        setOutputFormat(OUTPUT_FORMAT);
    }

    public GetlineCompleter getCompleter() {
        return this.completer;
    }

    protected boolean validateArgs(String[] args) {
        return args.length <= 1;
    }

    public String getSyntaxArgs() {
        return "[filesystem]";
    }

    public String getUsageShort() {
        return "Report filesystem disk i/o";
    }

    public void printHeader() {
        printf(HEADER);
    }

    public void output(String[] args) throws SigarException {
        if (args.length == 1) {
            FileSystemMap mounts = this.proxy.getFileSystemMap();
            String name = FileCompleter.expand(args[0]);
            FileSystem fs = mounts.getMountPoint(name);

            if (fs != null) {
                printHeader();
                output(fs);
                return;
            }

            throw new SigarException(args[0] +
                                     " No such file or directory");
        }
        else {
            FileSystem[] fslist = this.proxy.getFileSystemList();
            printHeader();
            for (int i=0; i<fslist.length; i++) {
                if (fslist[i].getType() == FileSystem.TYPE_LOCAL_DISK) {
                    output(fslist[i]);
                }
            }
        }
    }

    public void output(FileSystem fs) throws SigarException {
        FileSystemUsage usage =
            this.sigar.getFileSystemUsage(fs.getDirName());

        ArrayList items = new ArrayList();

        items.add(fs.getDevName());
        items.add(fs.getDirName());
        items.add(String.valueOf(usage.getDiskReads()));
        items.add(String.valueOf(usage.getDiskWrites()));

        if (usage.getDiskReadBytes() == Sigar.FIELD_NOTIMPL) {
            items.add("-");
            items.add("-");
        }
        else {
            items.add(Sigar.formatSize(usage.getDiskReadBytes()));
            items.add(Sigar.formatSize(usage.getDiskWriteBytes()));
        }

        if (usage.getDiskQueue() == Sigar.FIELD_NOTIMPL) {
            items.add("-");
        }
        else {
            items.add(String.valueOf(usage.getDiskQueue()));
        }

        printf(items);
    }

    public static void main(String[] args) throws Exception {
        new Iostat().processCommand(args);
    }
}
