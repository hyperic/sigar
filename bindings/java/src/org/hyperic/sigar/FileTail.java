package org.hyperic.sigar;

import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.HashMap;

public abstract class FileTail extends FileWatcher {

    public static final String PROP_USE_SUDO =
        "sigar.tail.sudo";

    private boolean useSudo = 
        "true".equals(System.getProperty(PROP_USE_SUDO));

    private HashMap offsets = new HashMap();
    
    public abstract void tail(FileInfo info, Reader reader);

    public FileTail(Sigar sigar) {
        super(sigar);
    }

    public void useSudo(boolean useSudo) {
        this.useSudo = useSudo;
    }

    public void onChange(FileInfo info) {
        Reader reader = null;
        String name = info.getName();

        try {
            if (this.useSudo) {
                reader =
                    new InputStreamReader(new SudoFileInputStream(name));
            }
            else {
                reader = new FileReader(name);
            }
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
