package net.hyperic.sigar;

class SigarCacheObject {

    int expire = SigarProxyCache.EXPIRE_DEFAULT;
    long timestamp = 0;
    Object value = null;
}
