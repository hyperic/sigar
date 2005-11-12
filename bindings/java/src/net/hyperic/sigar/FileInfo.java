package net.hyperic.sigar;

import java.util.Date;

public class FileInfo extends FileAttrs {

    String name;
    private Sigar sigar;
    private boolean dirStatEnabled = false;
    private DirStat stat = null;
    private boolean lstat;
    private FileInfo oldInfo = null;

    private static char DIFF_SEP = '|';

    /**
     * No file type determined.
     */
    public static final int TYPE_NOFILE  = 0;
    /**
     * A regular file.
     */
    public static final int TYPE_REG     = 1;
    /**
     * A directory.
     */
    public static final int TYPE_DIR     = 2;
    /**
     * A character device.
     */
    public static final int TYPE_CHR     = 3;
    /**
     * A block device.
     */
    public static final int TYPE_BLK     = 4;
    /**
     * A FIFO / pipe.
     */
    public static final int TYPE_PIPE    = 5;
    /**
     * A symbolic link.
     */
    public static final int TYPE_LNK     = 6;
    /**
     * A [unix domain] socket.
     */
    public static final int TYPE_SOCK    = 7;
    /**
     * A file of unknown type.
     */
    public static final int TYPE_UNKFILE = 8;

    /**
     * Readable by user.
     */
    public static final int MODE_UREAD    =  0x0400;
    /**
     * Writable by user.
     */
    public static final int MODE_UWRITE   =  0x0200;
    /**
     * Executable by user.
     */
    public static final int MODE_UEXECUTE =  0x0100;

    /**
     * Readable by group.
     */
    public static final int MODE_GREAD    =  0x0040;
    /**
     * Writable by group.
     */
    public static final int MODE_GWRITE   =  0x0020;
    /**
     * Executable by group.
     */
    public static final int MODE_GEXECUTE =  0x0010;

    /**
     * Readable by others.
     */
    public static final int MODE_WREAD    =  0x0004;
    /**
     * Writable by others.
     */
    public static final int MODE_WWRITE   =  0x0002;
    /**
     * Executable by others.
     */
    public static final int MODE_WEXECUTE =  0x0001;

    private static native String getTypeString(int type);

    native void gatherLink(Sigar sigar, String name)
        throws SigarException;

    public String getTypeString() {
        return FileInfo.getTypeString(this.type);
    }

    public char getTypeChar() {
        switch (this.type) {
          case TYPE_DIR:
            return 'd';
          case TYPE_CHR:
            return 'c';
          case TYPE_BLK:
            return 'b';
          case TYPE_PIPE:
            return 'p';
          case TYPE_LNK:
            return 'l';
          case TYPE_SOCK:
            return 's';
          default:
            return '-';
        }
    }

    public String getName() {
        return this.name;
    }

    public int hashCode() {
        return this.name.hashCode();
    }

    public boolean equals(Object o) {
        return o.equals(this.name);
    }

    private static native String getPermissionsString(long type);

    public String getPermissionsString() {
        return FileInfo.getPermissionsString(this.permissions);
    }

    private static native int getMode(long permissions);

    /**
     * Convert permissions bit mask to human readable number.
     * Example:
     * <code>MODE_UREAD|MODE_UWRITE|MODE_GREAD|MODE_WREAD</code>
     * converts to <code>644</code>.
     * @return The file permissions mode.
     */
    public int getMode() {
        return FileInfo.getMode(this.permissions);
    }

    public void enableDirStat(boolean value) {
        this.dirStatEnabled = value;
        if (value) {
            if (this.type != TYPE_DIR) {
                throw new IllegalArgumentException(this.name + " is not a directory");
            }

            try {
                if (this.stat == null) {
                    this.stat = this.sigar.getDirStat(this.name);
                }
                else {
                    this.stat.gather(this.sigar, this.name);
                }
            } catch (SigarException e) {
                //ok for now
            }
        }
    }

    public String diff() {
        if (this.oldInfo == null) {
            return "";
        }
        return diff(this.oldInfo);
    }

    public String diff(DirStat stat) {
        DirStat thisStat = this.stat;
        StringBuffer sb = new StringBuffer();

        if (thisStat.files != stat.files) {
            sb.append("Files........").
                append(stat.files).
                append(DIFF_SEP).
                append(thisStat.files).
                append("\n");
        }

        if (thisStat.subdirs != stat.subdirs) {
            sb.append("Subdirs......").
                append(stat.subdirs).
                append(DIFF_SEP).
                append(thisStat.subdirs).
                append("\n");
        }

        if (thisStat.symlinks != stat.symlinks) {
            sb.append("Symlinks.....").
                append(stat.symlinks).
                append(DIFF_SEP).
                append(thisStat.symlinks).
                append("\n");
        }

        if (thisStat.chrdevs != stat.chrdevs) {
            sb.append("Chrdevs......").
                append(stat.chrdevs).
                append(DIFF_SEP).
                append(thisStat.chrdevs).
                append("\n");
        }

        if (thisStat.blkdevs != stat.blkdevs) {
            sb.append("Blkdevs......").
                append(stat.blkdevs).
                append(DIFF_SEP).
                append(thisStat.blkdevs).
                append("\n");
        }

        if (thisStat.sockets != stat.sockets) {
            sb.append("Sockets......").
                append(stat.sockets).
                append(DIFF_SEP).
                append(thisStat.sockets).
                append("\n");
        }

        if (thisStat.total != stat.total) {
            sb.append("Total........").
                append(stat.total).
                append(DIFF_SEP).
                append(thisStat.total).
                append("\n");
        }

        return sb.toString();
    }

    public String diff(FileInfo info) {
        StringBuffer sb = new StringBuffer();
        boolean changed = false;

        if (this.ctime != info.ctime) {
            sb.append("Ctime........").
                append(new Date(info.ctime)).
                append(DIFF_SEP).
                append(new Date(this.ctime)).
                append("\n");
            changed = true;
        }

        if (this.mtime != info.mtime) {
            sb.append("Mtime........").
                append(new Date(info.mtime)).
                append(DIFF_SEP).
                append(new Date(this.mtime)).
                append("\n");
        }
        else if (!changed) {
            //no point in checking the rest if all times are the same.
            //or should we include atime in the diff?
            return "";
        }

        if (this.atime != info.atime) {
            sb.append("Atime........").
                append(new Date(info.atime)).
                append(DIFF_SEP).
                append(new Date(this.atime)).
                append("\n");
        }

        if (this.permissions != info.permissions) {
            sb.append("Permissions..").
                append(getPermissionsString(info.permissions)).
                append(DIFF_SEP).
                append(getPermissionsString(this.permissions)).
                append("\n");
        }

        if (this.type != info.type) {
            sb.append("Type.........").
                append(getTypeString(info.type)).
                append(DIFF_SEP).
                append(getTypeString(this.type)).
                append("\n");
        }

        if (this.uid != info.uid) {
            sb.append("Uid..........").
                append(info.uid).
                append(DIFF_SEP).
                append(this.uid).
                append("\n");
        }

        if (this.gid != info.gid) {
            sb.append("Gid..........").
                append(info.gid).
                append(DIFF_SEP).
                append(this.gid).
                append("\n");
        }

        if (this.inode != info.inode) {
            sb.append("Inode........").
                append(info.inode).
                append(DIFF_SEP).
                append(this.inode).
                append("\n");
        }

        if (this.device != info.device) {
            sb.append("Device.......").
                append(info.device).
                append(DIFF_SEP).
                append(this.device).
                append("\n");
        }

        if (this.nlink != info.nlink) {
            sb.append("Nlink........").
                append(info.nlink).
                append(DIFF_SEP).
                append(this.nlink).
                append("\n");
        }

        if (this.size != info.size) {
            sb.append("Size.........").
                append(info.size).
                append(DIFF_SEP).
                append(this.size).
                append("\n");
        }

        if (this.dirStatEnabled) {
            sb.append(diff(info.stat));
        }

        return sb.toString();
    }

    public FileInfo getPreviousInfo() {
        return this.oldInfo;
    }

    public boolean modified()
        throws SigarException,
               SigarFileNotFoundException {

        if (this.oldInfo == null) {
            this.oldInfo = new FileInfo();
            if (this.dirStatEnabled) {
                this.oldInfo.stat = new DirStat();
            }
        }
        copyTo(this.oldInfo);
        if (this.dirStatEnabled) {
            this.stat.copyTo(this.oldInfo.stat);
        }

        stat();

        return this.mtime != oldInfo.mtime;
    }

    public boolean changed()
        throws SigarException,
               SigarFileNotFoundException {

        return modified() || (this.ctime != oldInfo.ctime);
    }

    public void stat()
        throws SigarException,
               SigarFileNotFoundException {

        long mtime = this.mtime;

        if (this.lstat) {
            this.gatherLink(this.sigar, this.name);
        }
        else {
            this.gather(this.sigar, this.name);
        }

        if (this.dirStatEnabled &&
            (mtime != this.mtime)) //no need to fetch stat if unmodified.
        {
            this.stat.gather(this.sigar, this.name);
        }
    }

    private static FileInfo fetchInfo(Sigar sigar, String name,
                                      boolean followSymlinks)
        throws SigarException {

        FileInfo info = new FileInfo();

        if (followSymlinks) {
            info.gather(sigar, name);
            info.lstat = false;
        }
        else {
            info.gatherLink(sigar, name);
            info.lstat = true;
        }

        info.sigar = sigar;
        info.name = name;
        return info;
    }

    static FileInfo fetchFileInfo(Sigar sigar, String name)
        throws SigarException {

        return fetchInfo(sigar, name, true);
    }

    static FileInfo fetchLinkInfo(Sigar sigar, String name)
        throws SigarException {

        return fetchInfo(sigar, name, false);
    }
}
