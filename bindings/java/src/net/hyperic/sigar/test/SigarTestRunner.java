package net.hyperic.sigar.test;

import java.util.ArrayList;
import java.util.Collection;

import net.hyperic.sigar.SigarException;

import net.hyperic.sigar.cmd.SigarCommandBase;
import net.hyperic.sigar.cmd.Shell;

public class SigarTestRunner extends SigarCommandBase {

    private Collection completions;

    private static final Class[] TESTS = {
        TestLog.class,
        TestInvoker.class,
        TestPTQL.class,
        TestCpu.class,
        TestCpuInfo.class,
        TestFileInfo.class,
        TestFileSystem.class,
        TestFQDN.class,
        TestLoadAverage.class,
        TestMem.class,
        TestNetIf.class,
        TestProcArgs.class,
        TestProcEnv.class,
        TestProcExe.class,
        TestProcModules.class,
        TestProcFd.class,
        TestProcList.class,
        TestProcMem.class,
        TestProcState.class,
        TestProcStat.class,
        TestProcTime.class,
        TestSwap.class,
        TestThreadCpu.class,
        TestUptime.class,
    };

    public SigarTestRunner(Shell shell) {
        super(shell);

        this.completions = new ArrayList();
        for (int i=0; i<TESTS.length; i++) {
            String name = TESTS[i].getName();
            int ix = name.lastIndexOf(".Test");
            this.completions.add(name.substring(ix + 5));
        }
    }

    public SigarTestRunner() {
        super();
    }

    protected boolean validateArgs(String[] args) {
        return true;
    }

    public String getSyntaxArgs() {
        return "[testclass]";
    }

    public String getUsageShort() {
        return "Run sigar tests";
    }

    public Collection getCompletions() {
        return this.completions;
    }

    public void output(String[] args) throws SigarException {
        SigarTestPrinter.runTests(TESTS, args);
    }

    public static void main(String[] args) throws Exception {
        new SigarTestRunner().processCommand(args);
    }
}
