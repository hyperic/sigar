package org.hyperic.sigar.ptql;

import org.hyperic.sigar.ProcCred;
import org.hyperic.sigar.ProcState;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarProxy;

public class ProcessQueryGenerator {

    private ProcessFinder finder;
    private SigarProxy sigar;

    public ProcessQueryGenerator(SigarProxy sigar) {
        this.sigar = sigar;
        this.finder = new ProcessFinder(sigar);
    }

    public String generate(long pid)
        throws SigarException {

        StringBuffer query = new StringBuffer();

        ProcState state = sigar.getProcState(pid);
        query.append("State.Name.eq=" + state.getName());

        if (this.finder.find(query).length == 1) {
            return query.toString();
        }

        try {
            ProcCred cred = sigar.getProcCred(pid);
            query.append(",").append("Cred.Uid.eq=" + cred.getUid());
            query.append(",").append("Cred.Gid.eq=" + cred.getGid());

            if (this.finder.find(query).length == 1) {
                return query.toString();
            }
        } catch (SigarException e) {
        }

        try {
            String[] args = sigar.getProcArgs(pid);
            for (int i=args.length-1; i>=0; i--) {
                int j;
                //common case for java apps, last arg is the classname
                //use -1 for query since number of args may change,
                //but last arg is always the classname.
                if (i == args.length-1) {
                    j = -1;
                }
                else {
                    j = i;
                }
                query.append(",").append("Args." + j + ".eq=" + args[i]);

                if (this.finder.find(query).length == 1) {
                    return query.toString();
                }
            }
        } catch (SigarException e) {
        }

        return null;
    }
}
