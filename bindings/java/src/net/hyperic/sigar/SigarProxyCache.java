package net.hyperic.sigar;

import java.util.Map;

import java.lang.reflect.Proxy;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import net.hyperic.sigar.util.ReferenceMap;

/**
 * This class implements a caching mechanism to avoid system calls
 * with heavy Sigar usage in a short period of time.  It is intended
 * for use in applications such as Top.
 */
public class SigarProxyCache
    implements InvocationHandler {

    private Sigar sigar;
    private Map cache = ReferenceMap.newInstance();
    public static final int EXPIRE_DEFAULT = 30 * 1000; //30 seconds
    private int expire;
    private static final boolean debugEnabled =
        "debug".equals(System.getProperty("sigar.log"));

    public SigarProxyCache(Sigar sigar, int expire) {
        this.sigar = sigar;
        this.expire = expire;
    }

    public static SigarProxy newInstance() {
        return newInstance(new Sigar());
    }

    public static SigarProxy newInstance(Sigar sigar) {
        return newInstance(sigar, EXPIRE_DEFAULT);
    }

    public static SigarProxy newInstance(Sigar sigar, int expire) {

        SigarProxyCache handler = new SigarProxyCache(sigar, expire);
        SigarProxy proxy;

        proxy = (SigarProxy)
            Proxy.newProxyInstance(SigarProxy.class.getClassLoader(),
                                   new Class[] { SigarProxy.class },
                                   handler);

        return proxy;
    }

    //poor mans logging
    private void debug(String msg) {
        System.out.println("[DEBUG] SigarProxyCache - " + msg);
    }

    private static final Class[] VOID_SIGNATURE = new Class[0];

    public static void setExpire(SigarProxy proxy,
                                 String type,
                                 int expire)
        throws SigarException {

        SigarProxyCache handler =
            (SigarProxyCache)Proxy.getInvocationHandler(proxy);

        Method method;

        try {
            method = Sigar.class.getMethod("get" + type, VOID_SIGNATURE);
        } catch (Exception e) {
            throw new SigarException("invalid type " + type);
        }

        SigarCacheObject cacheVal =
            (SigarCacheObject)handler.cache.get(method);

        if (cacheVal == null) {
            cacheVal = new SigarCacheObject();
        }

        cacheVal.expire = expire;

        handler.cache.put(method, cacheVal);
    }

    public static void clear(Object proxy) {
        SigarProxyCache handler = 
            (SigarProxyCache)Proxy.getInvocationHandler(proxy);
        handler.cache.clear();
    }

    private String getDebugArgs(Object[] args, Object argKey) {

        if (args.length == 0) {
            return null;
        }

        StringBuffer dargs =
            new StringBuffer(args[0].toString());
        
        for (int i=1; i<args.length; i++) {
            dargs.append(',').append(args[i].toString());
        }

        if (!dargs.toString().equals(argKey.toString())) {
            dargs.append('/').append(argKey);
        }

        return dargs.toString();
    }

    /**
     * The java.lang.reflect.InvocationHandler used by the Proxy.
     * This method handles caching of all Sigar type objects.
     */
    public Object invoke(Object proxy, Method method, Object[] args)
        throws SigarException, SigarNotImplementedException {

        SigarCacheObject cacheVal = null;
        Object retval;
        Object argKey = null;
        Map argMap = null;
        long timeNow = System.currentTimeMillis();

        if (args != null) {
            if (args.length == 1) {
                argKey = args[0];
            }
            else {
                int hashCode = 0;
                for (int i=0; i<args.length; i++) {
                    hashCode ^= args[i].hashCode();
                }
                argKey = new Integer(hashCode);
            }
            argMap = (Map)this.cache.get(method);
            if (argMap == null) {
                argMap = ReferenceMap.newInstance();
            }
            else {
                //XXX what todo when pids are stale?
                cacheVal = (SigarCacheObject)argMap.get(argKey);
            }
        }
        else {
            cacheVal = (SigarCacheObject)this.cache.get(method);
        }

        if (cacheVal == null) {
            cacheVal = new SigarCacheObject();
            cacheVal.expire = this.expire;
        }

        if (cacheVal.value != null) {
            String argDebug = "";

            if (this.debugEnabled) {
                if ((args != null) && (args.length != 0)) {
                    argDebug = " with args=" +
                        getDebugArgs(args, argKey);
                }

                debug("found " + method.getName() +
                      " in cache" + argDebug);
            }

            if ((timeNow - cacheVal.timestamp) > cacheVal.expire) {
                if (this.debugEnabled) {
                    debug("expiring " + method.getName() +
                          " from cache" + argDebug);
                }

                cacheVal.value = null;
            }
        }

        if (cacheVal.value == null) {
            try {
                retval = method.invoke(this.sigar, args);
            } catch (InvocationTargetException e) {
                Throwable t =
                    ((InvocationTargetException)e).
                    getTargetException();

                String msg;

                if (t instanceof SigarException) {
                    msg = "";
                }
                else {
                    msg = t.getClass().getName() + ": ";
                }

                msg += t.getMessage();

                if (argKey != null) {
                    msg += ": " + getDebugArgs(args, argKey);
                }

                if (t instanceof SigarNotImplementedException) {
                    throw new SigarNotImplementedException(msg);
                }
                throw new SigarException(msg);
            } catch (Exception e) {
                String msg =
                    e.getClass().getName() + ": " +
                    e.getMessage();

                if (argKey != null) {
                    msg += ": " + getDebugArgs(args, argKey);
                }

                throw new SigarException(msg);
            }

            cacheVal.value = retval;
            cacheVal.timestamp = timeNow;

            if (args == null) {
                this.cache.put(method, cacheVal);
            }
            else {
                argMap.put(argKey, cacheVal);
                this.cache.put(method, argMap);
            }
        }
        else {
            retval = cacheVal.value;
        }

        return retval;
    }
}
