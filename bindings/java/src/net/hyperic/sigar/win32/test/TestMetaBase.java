package net.hyperic.sigar.win32.test;

import junit.framework.TestCase;

public class TestMetaBase extends TestCase {

    public TestMetaBase(String name) {
        super(name);
    }

    /**
     * TODO: This test shouldn't fail if IIS is not installed.  If
     *       the exceptions thrown from the native code were actually
     *       useful, we could do this.  Re-add this test when the
     *       exceptions are cleaned up.
     **/
    public void testMetaBase() throws Exception {
    }
}
