package net.hyperic.sigar.ptql;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarException;
import net.hyperic.sigar.SigarNotImplementedException;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarProxyCache;

public class ProcessFinder {

    private SigarProxy proxy;

    public ProcessFinder(SigarProxy proxy) {
        this.proxy = proxy;
        //getpid() cache to optimize queries on this process.
        this.proxy.getPid();
    }

    public long findSingleProcess(ProcessQuery query)
        throws SigarException, SigarNotImplementedException,
        MalformedQueryException {

        if (query instanceof PidQuery) {
            return ((PidQuery)query).getPid();
        }

        int i, matches = 0;

        long[] pids = this.proxy.getProcList();
        long pid=-1;

        for (i=0; i<pids.length; i++) {
            try {
                if (query.match(this.proxy, pids[i])) {
                    matches++;
                    pid = pids[i];
                }
            } catch (SigarNotImplementedException e) {
                throw e; //let caller know query is invalid.
            } catch (SigarException e) {
                //ok, e.g. permission denied.
            }
        }

        if (matches == 1) {
            return pid;
        }

        String msg;

        if (matches == 0) {
            msg = "Query did not match any processes";
        }
        else {
            msg = "Query matched multiple processes" +
                  " (" + matches + ")";
        }

        throw new MalformedQueryException(msg);
    }

    public static long[] find(Sigar sigar, String query)
        throws SigarException {

        SigarProxy proxy = 
            SigarProxyCache.newInstance(sigar);

        return find(proxy, query);
    }

    public static long[] find(SigarProxy sigar, String query)
        throws SigarException {

        ProcessFinder finder = new ProcessFinder(sigar);

        try {
            return finder.find(ProcessQueryFactory.getInstance(query, sigar));
        } catch (QueryLoadException e) {
            throw new SigarException(e.getMessage());
        } catch (MalformedQueryException e) {
            throw new SigarException(e.getMessage());
        }
    }

    public long[] find(StringBuffer query)
        throws SigarException, SigarNotImplementedException {

        return find(query.toString());
    }

    public long[] find(String query)
        throws SigarException, SigarNotImplementedException {

        try {
            return find(ProcessQueryFactory.getInstance(query));
        } catch (QueryLoadException e) {
            throw new SigarException(e.getMessage());
        } catch (MalformedQueryException e) {
            throw new SigarException(e.getMessage());
        }
    }

    public long[] find(ProcessQuery query)
        throws SigarException, SigarNotImplementedException {

        int matches=0, lastMatch=-1;

        //because we modify the array below.  XXX this sucks.
        long[] pids = (long[])this.proxy.getProcList().clone();

        for (int i=0; i<pids.length; i++) {
            long pid = pids[i];
            boolean add = false;

            try {
                add = query.match(this.proxy, pid);
            } catch (SigarNotImplementedException e) {
                throw e; //let caller know query is invalid.
            } catch (SigarException e) {
                //ok, e.g. permission denied.
            }

            if (add) {
                ++matches;
                lastMatch = i;
            }
            else {
                pids[i] = -1;
                continue;
            }
        }

        long[] matched = new long[matches];

        if (matches == 1) {
            /* avoid loop below */
            matched[0] = pids[lastMatch];
            return matched;
        }

        //XXX this is clunky
        for (int i=0, j=0; i<=lastMatch; i++) {
            if (pids[i] == -1) {
                continue;
            }
            matched[j++] = pids[i];
        }

        return matched;
    }
}
    
