package net.hyperic.sigar.test;

import java.io.PrintStream;
import java.util.HashMap;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestFailure;
import junit.framework.TestSuite;

import junit.textui.ResultPrinter;
import junit.textui.TestRunner;

import net.hyperic.sigar.cmd.Version;

public class SigarTestPrinter extends ResultPrinter {

    private static String PACKAGE_NAME = 
        SigarTestCase.class.getPackage().getName();

    private HashMap failures = new HashMap();
    private int maxNameLen = 0;

    public SigarTestPrinter(PrintStream writer) {
        super(writer);
    }

    //output similar to perl TestHarness
    //more interesting useful than the default '.', 'F', 'E'
    //for each test success, failure or error.
    public void startTest(Test test) {
        PrintStream writer = getWriter();
        String name = test.getClass().getName();
        writer.print(name);
        int n = ((maxNameLen+3) - name.length());
        for (int i=0; i<n; i++) {
            writer.print('.');
        }
    }

    public void addFailure(Test test, AssertionFailedError t) {
        this.failures.put(test, Boolean.TRUE);
        getWriter().println("FAILED");
    }

    public void addError(Test test, Throwable t) {
        this.failures.put(test, Boolean.TRUE);
        getWriter().println("ERROR");
    }

    public void endTest(Test test) {
        if (this.failures.get(test) != Boolean.TRUE) {
            getWriter().println("ok");
        }
    }

    protected void printDefectHeader(TestFailure failure, int count) {
        getWriter().println(count + ") " +
                            failure.failedTest().getClass().getName() + ":");
    }

    public void printVersionInfo() {
        PrintStream writer = getWriter();

        Version.printInfo(writer);

        writer.println("");
    }

    public static void addTest(SigarTestPrinter printer,
                               TestSuite suite,
                               Class test) {

        int len = test.getName().length();

        if (len > printer.maxNameLen) {
            printer.maxNameLen = len;
        }

        suite.addTestSuite(test);
    }

    public static void runTests(Class[] tests, String[] args) {
        TestSuite suite = new TestSuite("Sigar tests");

        SigarTestPrinter printer = new SigarTestPrinter(System.out);

        printer.printVersionInfo();

        if (args.length > 0) {
            SigarTestCase.setVerbose(true);
            SigarTestCase.setWriter(printer.getWriter());

            for (int i=0; i<args.length; i++) {
                Class test;
                try {
                    test = Class.forName(PACKAGE_NAME + ".Test" + args[i]);
                } catch (ClassNotFoundException e) {
                    String msg = "Invalid test: " + args[i];
                    throw new IllegalArgumentException(msg);
                }
                addTest(printer, suite, test);
            }
        }
        else {
            for (int i=0; i<tests.length; i++) {
                addTest(printer, suite, tests[i]);
            }
        }

        TestRunner runner = new TestRunner(printer);

        runner.doRun(suite);
    }
}
