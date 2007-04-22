/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

package org.hyperic.sigar.test;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.SigarNotImplementedException;

import org.hyperic.sigar.ptql.ProcessQuery;
import org.hyperic.sigar.ptql.ProcessQueryFactory;
import org.hyperic.sigar.ptql.MalformedQueryException;

public class TestPTQL extends SigarTestCase {

    private static final String THIS_PROCESS = "Pid.Pid.eq=$$";

    private ProcessQueryFactory qf;

    private static final String[] OK_QUERIES = {
        "State.Name.eq=java", //all java processs
        "Exe.Name.ew=java",   //similar
        "State.Name.eq=java,Exe.Cwd.eq=$user.dir", //process running this test
        "State.Name.eq=java,Exe.Cwd.eq=$PWD", //getenv
        "State.Name.ne=java,Exe.Cwd.eq=$user.dir", //parent(s) of process running this test
        "State.Name.sw=httpsd,State.Name.Pne=$1", //httpsd parent process
        "State.Name.ct=ssh", //anything ssh, "ssh", "ssh-agent", "sshd"
        "State.Name.eq=java,Args.-1.ew=AgentClient", //hq agents
        "Cred.Uid.eq=1003,State.Name.eq=java,Args.-1.ew=AgentClient", //my hq agent
        "Cred.Uid.gt=0,Cred.Uid.lt=1000", //range of users
        "Cred.Uid.eq=1003,Cred.Gid.eq=1003", //me
        "CredName.User.eq=dougm", //me
        "Time.Sys.gt=1000", //cpu hog
        "Fd.Total.gt=20", //lots of open files
        "Mem.Size.ge=10000000,Mem.Share.le=1000000", //memory hog
        "State.Name.eq=sshd,Cred.Uid.eq=0",
        "State.Name.eq=crond,Cred.Uid.eq=0",
        "State.State.eq=R", //processes in read state
        "Args.0.eq=sendmail: accepting connections",
        "Args.0.sw=sendmail: Queue runner@",
        "Args.1000.eq=foo",
        "Args.*.eq=org.apache.tools.ant.Main", //'*' == any arg
        "Args.*.ct=java", //'*' == any arg
        "Args.*.ew=sigar.jar", //'*' == any arg
        "Port.tcp.eq=80,Cred.Uid.eq=0", //root owned http port
        "Port.udp.eq=161,Cred.Uid.eq=0", //root owned snmp port
        "Port.tcp.eq=8080,Cred.Uid.eq=1003", //dougm owned jboss port
        "Pid.PidFile.eq=pid.file",
        "Pid.Pid.eq=1",
        THIS_PROCESS,
        "Pid.Service.eq=Eventlog",
        "Pid.Service.eq=NOSUCHSERVICE",
        "Pid.Service.eq=Hyperic HQ Agent",
        "State.Name.eq=java,Pid.Pid.ne=$$", //all java procs cept this one
        "Cpu.Percent.ge=0.2",
        "State.Name.sw=java,Args.*.eq=org.jboss.Main", //jboss
        "State.Name.eq=java,Args.*.eq=com.ibm.ws.runtime.WsServer", //websphere
        "State.Name.eq=java,Args.-1.eq=weblogic.Server", //weblogic
    };

    //XXX current required 1.4+
    private static final String[] OK_RE_QUERIES = {
        "Args.-1.eq=weblogic.Server,Env.WEBLOGIC_CLASSPATH.re=.*weblogic.jar.*", //weblogic
        "State.Name.re=https?d.*|[Aa]pache2?$,State.Name.Pne=$1", //apache
        "State.Name.re=post(master|gres),State.Name.Pne=$1,Args.0.re=.*post(master|gres)$", //postgresql
    };

    private static final String[] MALFORMED_QUERIES = {
        "foo",
        "State.Name",
        "State.Name.eq",
        "State.Namex.eq=foo",
        "Statex.Name.eq=foo",
        "State.Name.eqx=foo",
        "State.Name.Xeq=foo",
        "State.Name.eq=$2",
        "State.State.eq=read",
        "Args.x.eq=foo",
        "Time.Sys.gt=x",
        "Pid.Pid.eq=foo",
        "Cpu.Percent.ge=x",
        "Port.foo.eq=8080",
        "",
        null,
        //disabled for testing w/ -Dsigar.ptql.native=true
        //where these do not throw an exception
        //"Pid.PidFile.ne=pid.file",
        //"Pid.Service.ne=Eventlog",
    };

    public TestPTQL(String name) {
        super(name);
        this.qf = new ProcessQueryFactory();
    }

    private int runQuery(Sigar sigar, String qs)
        throws MalformedQueryException,
               SigarException {

        ProcessQuery query;
        try {
            query = this.qf.getQuery(qs);
        } catch (MalformedQueryException e) {
            traceln("parse error: " + qs);
            throw e;
        }

        try {
            long[] pids = query.find(sigar);

            traceln(pids.length + " processes match: " + qs);

            if (pids.length == 1) {
                long pid = query.findProcess(sigar);
                assertTrue(pid + "==" + pids[0],
                           pid == pids[0]);
            }
            return pids.length;
        } catch (SigarNotImplementedException e) {
            return 0;
        }
    }

    private void testOK(Sigar sigar) throws Exception {
        assertTrue(THIS_PROCESS,
                   runQuery(sigar, THIS_PROCESS) == 1);

        for (int i=0; i<OK_QUERIES.length; i++) {
            String qs = OK_QUERIES[i];
            assertTrue(qs,
                       runQuery(sigar, qs) >= 0);
        }
        this.qf.clear();
    }

    private void testReOK(Sigar sigar) throws Exception {
        for (int i=0; i<OK_RE_QUERIES.length; i++) {
            String qs = OK_RE_QUERIES[i];
            assertTrue(qs,
                       runQuery(sigar, qs) >= 0);
        }
        this.qf.clear();
    }

    private void testMalformed(Sigar sigar) throws Exception {
        for (int i=0; i<MALFORMED_QUERIES.length; i++) {
            String qs = MALFORMED_QUERIES[i];
            try {
                runQuery(sigar, qs);
                fail("'" + qs + "' did not throw MalformedQueryException");
            } catch (MalformedQueryException e) {
                traceln(qs + ": " + e.getMessage());
                assertTrue(qs + " Malformed", true);
            }
        }
        this.qf.clear();
    }

    public void testCreate() throws Exception {
        testOK(getSigar());

        if (JDK_14_COMPAT) {
            testReOK(getSigar());
        }

        testMalformed(getSigar());
    }
}

