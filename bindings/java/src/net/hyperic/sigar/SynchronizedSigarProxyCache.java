package net.hyperic.sigar;

import java.lang.reflect.Method;

public class SynchronizedSigarProxyCache
    extends SigarProxyCache {

    private static Object lock = new Object();
    private static SigarProxy instance = null;

    public static SigarProxy getInstance()
        throws SigarException {

        synchronized (lock) {
            if (instance == null) {
                instance = SigarProxyCache.newInstance(new Sigar());
            }
        }

        return instance;
    }

    public SynchronizedSigarProxyCache(Sigar sigar, int expire) {
        super(sigar, expire);
    }

    //all Objects returned are read-only.
    public synchronized Object invoke(Object proxy, Method method, Object[] args)
        throws SigarException, SigarNotImplementedException {

        return super.invoke(proxy, method, args);
    }
}
