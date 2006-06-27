package org.hyperic.sigar.ptql;

class ProcessQueryClassLoader extends ClassLoader {
    private byte[] bytecode;

    ProcessQueryClassLoader(ClassLoader parent, byte[] bytecode) {
        super(parent);
        this.bytecode = bytecode;
    }

    public Class findClass(String name) throws ClassNotFoundException {
        try {
            return defineClass(name, this.bytecode, 0, this.bytecode.length);
        } catch (ClassFormatError e) {
            throw new ClassNotFoundException("Class Format Error", e);
        }
    }
}
