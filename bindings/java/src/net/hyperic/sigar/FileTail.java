package net.hyperic.sigar;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;

public abstract class FileTail extends FileWatcher {

    private HashMap offsets = new HashMap();
    
    public abstract void tail(FileInfo info, Reader reader);

    public FileTail(Sigar sigar) {
        super(sigar);
    }

    public void onChange(FileInfo info) {
        long len = info.size;
 
        Reader reader = null;
               
        try {
            reader = new FileReader(info.getName());
            reader.skip(getOffset(info));
            tail(info, reader);
            setOffset(info);
        } catch (IOException e) {
            //XXX
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try { reader.close(); } catch (IOException e) { }
            }
        }
    }

    public FileInfo add(String file)
        throws SigarException {
        FileInfo info = super.add(file);
        setOffset(info);
        return info;
    }

    protected boolean changed(FileInfo info)
        throws SigarException,
               SigarFileNotFoundException {

        return info.modified();
    }

    private long getOffset(FileInfo info) {
        Long offset = (Long)this.offsets.get(info);

        return offset.longValue();
    }

    private void setOffset(FileInfo info) {
        this.offsets.put(info, new Long(info.size));
    }
}
