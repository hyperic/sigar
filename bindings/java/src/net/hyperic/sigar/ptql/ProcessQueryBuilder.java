package net.hyperic.sigar.ptql;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import net.hyperic.sigar.SigarProxy;

import org.apache.bcel.Constants;
import org.apache.bcel.generic.BranchInstruction;
import org.apache.bcel.generic.ClassGen;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.InstructionConstants;
import org.apache.bcel.generic.InstructionFactory;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InstructionList;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.PUSH;
import org.apache.bcel.generic.Type;

public class ProcessQueryBuilder {

    //set true during development to dump generated
    //.class files to disk.
    private static final boolean dumpClasses = false;

    private static int generation = 0;

    private static final String SIGAR_PACKAGE =
        "net.hyperic.sigar.";

    private static final String PROC_PREFIX =
        SIGAR_PACKAGE + "Proc";

    private static final String PROXY_CLASS =
        SIGAR_PACKAGE + "SigarProxy";

    private static final String PROXY_HELPER =
        SIGAR_PACKAGE + "ptql.ProcessQueryHelper";

    private static final String GENERATION_PACKAGE =
        SIGAR_PACKAGE + "ptql.GENERATED.";

    private static final String[] GENERATION_IMPL =
        new String[] { ProcessQuery.class.getName() };

    public static final Class[] NOPARAM =
        new Class[] {};

    static final char MOD_CLONE  = 'C';
    static final char MOD_PARENT = 'P';
    static final char MOD_VALUE  = 'V';

    private static final HashMap INUMOPS;
    private static final HashMap LNUMOPS;
    private static final HashMap STROPS;

    private static final HashMap IFMETHODS;

    private InstructionFactory factory;
    private ConstantPoolGen pool;
    private ClassGen generator;
    private String className;

    //ProcessQuery.match method impl
    private MethodGen query;
    //query instructions (match method body)
    private InstructionList qi = new InstructionList();
    //branches of query method
    private ArrayList branches = new ArrayList();

    public static final boolean COMPAT_1_4;

    static {
        //long
        HashMap lnops = new HashMap();
        lnops.put("eq", new Short(Constants.IFNE));
        lnops.put("ne", new Short(Constants.IFEQ));
        lnops.put("gt", new Short(Constants.IFGE));
        lnops.put("ge", new Short(Constants.IFGT));
        lnops.put("lt", new Short(Constants.IFLE));
        lnops.put("le", new Short(Constants.IFLT));
        LNUMOPS = lnops;

        //int
        HashMap inops = new HashMap();
        inops.put("eq", new Short(Constants.IF_ICMPNE));
        inops.put("ne", new Short(Constants.IF_ICMPEQ));
        inops.put("gt", new Short(Constants.IF_ICMPGE));
        inops.put("ge", new Short(Constants.IF_ICMPGT));
        inops.put("lt", new Short(Constants.IF_ICMPLE));
        inops.put("le", new Short(Constants.IF_ICMPLT));
        INUMOPS = inops;

        HashMap sops = new HashMap();
        sops.put("ew", new StringOp("endsWith", Constants.IFEQ));
        sops.put("sw", new StringOp("startsWith", Constants.IFEQ));
        sops.put("eq", new StringOp("equals", Constants.IFEQ));
        sops.put("ne", new StringOp("equals", Constants.IFNE));
        sops.put("re", new StringOp("matches", Constants.IFEQ));
        sops.put("ct", new StringOp("indexOf", Constants.IF_ICMPEQ));
        STROPS = sops;

        HashMap iftab = new HashMap();
        Method[] methods = SigarProxy.class.getMethods();

        for (int i=0; i<methods.length; i++) {
            String name = methods[i].getName();

            if (!name.startsWith("getProc")) {
                continue;
            }

            Class[] params = methods[i].getParameterTypes();

            //getProcFoo(long pid)
            if (!((params.length == 1) &&
                  (params[0] == Long.TYPE)))
            {
                continue;
            }

            iftab.put(name.substring(7), methods[i]);
        }
        IFMETHODS = iftab;

        boolean isCompat;

        try {
            Class.forName("java.net.SocketAddress");
            isCompat = true;
        } catch (ClassNotFoundException e) {
            isCompat = false;
        }

        COMPAT_1_4 = isCompat;
    }

    public ProcessQueryBuilder() {
        init();
    }

    public static Map getMethods() {
        return IFMETHODS;
    }

    public static Set getMethodOpNames(Method method) {
        if (method == null) {
            return STROPS.keySet();
        }

        Class rtype = method.getReturnType();

        if ((rtype == Character.TYPE) ||
            (rtype == Double.TYPE) ||
            (rtype == Integer.TYPE) ||
            (rtype == Long.TYPE))
        {
            return LNUMOPS.keySet();
        }

        return STROPS.keySet();
    }

    public static boolean isSigarClass(Class type) {
        return type.getName().startsWith(SIGAR_PACKAGE);
    }

    class QueryOp {
        String op;
        int flags;
        boolean isParent = false;
        boolean isClone = false;
        boolean isValue = false;

        public QueryOp(String val)
            throws MalformedQueryException {

            int i=0;
            char c;
            while (Character.isUpperCase((c = val.charAt(i)))) {
                switch (c) {
                  case MOD_CLONE:
                    this.isClone = true;
                    break;
                  case MOD_PARENT:
                    this.isParent = true;
                    break;
                  case MOD_VALUE:
                    this.isValue = true;
                    break;
                  default:
                    String msg = "Unsupported modifier: " + c;
                    throw new MalformedQueryException(msg);
                }
                i++;
            }

            this.op = val.substring(i);
        }
    }

    private void init() {
        String name = genClassName();
        this.className = GENERATION_PACKAGE + name;

        this.generator = 
            new ClassGen(this.className,
                         "java.lang.Object",
                         className + ".java",
                         Constants.ACC_PUBLIC|Constants.ACC_SUPER,
                         GENERATION_IMPL);

        this.pool = this.generator.getConstantPool();

        this.factory = new InstructionFactory(this.generator,
                                              this.pool);

        createConstructor();

        this.query =
            new MethodGen(Constants.ACC_PUBLIC,
                          Type.BOOLEAN,
                          new Type[] { new ObjectType(PROXY_CLASS), Type.LONG },
                          new String[] { "sigar", "pid" }, "match", getClassName(),
                          this.qi, this.pool);
    }

    //arg 1 passed to ProcessQuery.match
    private void loadSigarArg() {
        this.qi.append(InstructionFactory.createLoad(Type.OBJECT, 1));
    }

    //arg 2 passed to ProcessQuery.match
    private void loadPidArg() {
        this.qi.append(InstructionFactory.createLoad(Type.LONG, 2));
    }

    private static synchronized String genClassName() {
        return "PTQL" + generation++;
    }

    public String getClassName() {
        return this.className;
    }
    
    private void createConstructor() {
        InstructionList il = new InstructionList();

        MethodGen method =
            new MethodGen(Constants.ACC_PUBLIC,
                          Type.VOID, Type.NO_ARGS,
                          new String[] {}, "<init>",
                          getClassName(),
                          il, this.pool);

        il.append(InstructionFactory.createLoad(Type.OBJECT, 0));
        il.append(this.factory.createInvoke("java.lang.Object",
                                            "<init>",
                                            Type.VOID, Type.NO_ARGS,
                                            Constants.INVOKESPECIAL));

        il.append(InstructionFactory.createReturn(Type.VOID));
        method.setMaxStack();
        method.setMaxLocals();
        this.generator.addMethod(method.getMethod());
        il.dispose();
    }

    private void loadStandardArgs(QueryOp qop) {

        if (qop.isParent) {
            loadSigarArg();
        }

        loadSigarArg();
        loadPidArg();

        if (!qop.isParent) {
            return;
        }

        //e.g. sigar.getProcState(pid).getName() is converted to:
        //     sigar.getProcState(sigar.getProcState(pid).getPpid()).getName()
        final String procState = SIGAR_PACKAGE + "ProcState";

        this.qi.append(this.factory.createInvoke(PROXY_CLASS,
                                                 "getProcState",
                                                 new ObjectType(procState),
                                                 new Type[] { Type.LONG },
                                                 Constants.INVOKEINTERFACE));

        this.qi.append(this.factory.createInvoke(procState,
                                                 "getPpid",
                                                 Type.LONG, Type.NO_ARGS,
                                                 Constants.INVOKEVIRTUAL));
    }

    //e.g. sigar.getProcState(pid).getName()
    //attrClass == "State"
    //attr      == "Name"
    //type      == Type.STRING (return type)
    private void createStandardInvoker(String attrClass, String attr, Type type) {
        this.qi.append(this.factory.createInvoke(PROXY_CLASS,
                                                 "getProc" + attrClass,
                                                 new ObjectType(PROC_PREFIX + attrClass),
                                                 new Type[] { Type.LONG },
                                                 Constants.INVOKEINTERFACE));

        this.qi.append(this.factory.createInvoke(PROC_PREFIX + attrClass,
                                                 "get" + attr,
                                                 type, Type.NO_ARGS,
                                                 Constants.INVOKEVIRTUAL));
    }

    private MalformedQueryException unsupportedOp(String name) {
        return new MalformedQueryException("Unsupported operator: " +
                                           name);
    }

    private MalformedQueryException unsupportedMethod(String name) {
        return new MalformedQueryException("Unsupported method: " +
                                           name);
    }

    private MalformedQueryException unsupportedAttribute(String name) {
        return new MalformedQueryException("Unsupported attribute: " +
                                           name);
    }

    private StringOp getStringOp(String op)
        throws QueryLoadException,
               MalformedQueryException {

        StringOp sop = (StringOp)STROPS.get(op);

        if (sop == null) {
            throw unsupportedOp(op);
        }

        if (!COMPAT_1_4) {
            if (op.equals("re")) {
                throw new QueryLoadException(op + " requires jdk 1.4+");
            }
        }

        return sop;
    }

    private void createStringInvoker(String op)
        throws QueryLoadException,
               MalformedQueryException {

        StringOp sop = getStringOp(op);

        this.qi.append(this.factory.createInvoke("java.lang.String", sop.name,
                                                 sop.returnType, new Type[] { sop.type },
                                                 Constants.INVOKEVIRTUAL));

        if (op.equals("ct")) {
            this.qi.append(new PUSH(this.pool, -1));
        }

        BranchInstruction branch =
            InstructionFactory.createBranchInstruction(sop.opcode, null);
        this.qi.append(branch);

        this.branches.add(branch);
    }

    //special case
    public void appendProcPortOp(String flags, String op, long val)
        throws MalformedQueryException {

        //XXX flags current unused; could be used to narrow search scope.
        QueryOp qop = new QueryOp(op);

        loadSigarArg();

        this.qi.append(new PUSH(this.pool, val)); //port
        
        this.qi.append(this.factory.createInvoke(PROXY_CLASS,
                                                 "getProcPort",
                                                 Type.LONG,
                                                 new Type[] { Type.LONG },
                                                 Constants.INVOKEINTERFACE));

        loadPidArg();

        this.qi.append(InstructionConstants.LCMP);

        Short sop = (Short)LNUMOPS.get(qop.op);

        if (sop == null) {
            throw unsupportedOp(qop.op);
        }

        BranchInstruction branch =
            InstructionFactory.createBranchInstruction(sop.shortValue(), null);
        this.qi.append(branch);
        
        this.branches.add(branch);
    }

    //special case
    public void appendEnvOp(String key, String op, String val)
        throws QueryLoadException,
               MalformedQueryException {

        QueryOp qop = new QueryOp(op);

        loadStandardArgs(qop);

        this.qi.append(new PUSH(this.pool, key));

        this.qi.append(this.factory.createInvoke(PROXY_HELPER,
                                                 "getProcEnv", Type.STRING,
                                                 new Type[] {
                                                     new ObjectType(PROXY_CLASS),
                                                     Type.LONG, Type.STRING
                                                 },
                                                 Constants.INVOKESTATIC));

        this.qi.append(new PUSH(this.pool, val));

        createStringInvoker(qop.op);
    }

    //special case
    public void appendArgsOp(int idx, String op, String val)
        throws QueryLoadException,
               MalformedQueryException {

        QueryOp qop = new QueryOp(op);

        loadStandardArgs(qop);

        this.qi.append(new PUSH(this.pool, idx));

        this.qi.append(this.factory.createInvoke(PROXY_HELPER,
                                                 "getProcArg", Type.STRING,
                                                 new Type[] {
                                                     new ObjectType(PROXY_CLASS),
                                                     Type.LONG, Type.INT
                                                 },
                                                 Constants.INVOKESTATIC));

        this.qi.append(new PUSH(this.pool, val));

        createStringInvoker(qop.op);
    }

    public void appendArgsMatch(String op, String val)
        throws QueryLoadException,
               MalformedQueryException {

        QueryOp qop = new QueryOp(op);

        getStringOp(qop.op); //validate

        loadStandardArgs(qop);

        this.qi.append(new PUSH(this.pool, val));
        this.qi.append(new PUSH(this.pool, qop.op));

        this.qi.append(this.factory.createInvoke(PROXY_HELPER,
                                                 "argsMatch", Type.BOOLEAN,
                                                 new Type[] {
                                                     new ObjectType(PROXY_CLASS),
                                                     Type.LONG,
                                                     Type.STRING,
                                                     Type.STRING
                                                 },
                                                 Constants.INVOKESTATIC));

        BranchInstruction branch =
            InstructionFactory.createBranchInstruction(Constants.IFEQ, null);
        this.qi.append(branch);

        this.branches.add(branch);
    }

    public void appendStringOp(String attrClass, String attr,
                               String op, String val)
        throws QueryLoadException,
               MalformedQueryException {

        QueryOp qop = new QueryOp(op);

        loadStandardArgs(qop);

        createStandardInvoker(attrClass, attr, Type.STRING);

        if (qop.isValue) {
            return;
        }

        if (qop.isClone) {
            append(val, null);
        }
        else {
            this.qi.append(new PUSH(this.pool, val));
        }

        createStringInvoker(qop.op);
    }

    public void appendNumberOp(String attrClass, String attr,
                               String op, int val)
        throws MalformedQueryException {

        appendNumberOp(attrClass, attr, op, Type.INT,
                       0, 0.0, val);
    }

    public void appendNumberOp(String attrClass, String attr,
                               String op, long val)
        throws MalformedQueryException {

        appendNumberOp(attrClass, attr, op, Type.LONG,
                       val, 0.0, 0);
    }

    public void appendNumberOp(String attrClass, String attr,
                               String op, double val)
        throws MalformedQueryException {

        appendNumberOp(attrClass, attr, op, Type.DOUBLE,
                       0, val, 0);
    }

    private void appendNumberOp(String attrClass, String attr,
                                String op, Type type,
                                long val, double dval, int ival)
        throws MalformedQueryException {

        short opcode;
        HashMap nops;

        if ((type == Type.INT) ||
            (type == Type.CHAR))
        {
            nops = INUMOPS;
            this.qi.append(new PUSH(this.pool, ival));
        }
        else if (type == Type.DOUBLE) {
            nops = LNUMOPS;
            this.qi.append(new PUSH(this.pool, dval));
        }
        else {
            nops = LNUMOPS;
            this.qi.append(new PUSH(this.pool, val));
        }

        QueryOp qop = new QueryOp(op);

        loadStandardArgs(qop);

        createStandardInvoker(attrClass, attr, type);

        Short sop = (Short)nops.get(qop.op);

        if (sop == null) {
            throw unsupportedOp(qop.op);
        }

        if (type == Type.LONG) {
            this.qi.append(InstructionConstants.LCMP);
        }
        else if (type == Type.DOUBLE) {
            this.qi.append(InstructionConstants.DCMPL);
        }

        BranchInstruction branch =
            InstructionFactory.createBranchInstruction(sop.shortValue(), null);
        this.qi.append(branch);
        
        this.branches.add(branch);
    }

    private void appendPidOp(String op, String val)
        throws MalformedQueryException {

        long longVal;
        short opcode;
        HashMap nops = LNUMOPS;

        if (val.equals("$$")) {
            loadSigarArg();

            this.qi.append(this.factory.createInvoke(PROXY_CLASS,
                                                     "getPid",
                                                     Type.LONG, Type.NO_ARGS,
                                                     Constants.INVOKEINTERFACE));
        }
        else {
            try {
                longVal = Long.parseLong(val);
            } catch (NumberFormatException e) {
                String msg = "Pid value '" + val + "' is not a number";
                throw new MalformedQueryException(msg);
            }

            this.qi.append(new PUSH(this.pool, longVal));
        }

        loadPidArg();

        QueryOp qop = new QueryOp(op);

        Short sop = (Short)nops.get(qop.op);

        if (sop == null) {
            throw unsupportedOp(qop.op);
        }

        this.qi.append(InstructionConstants.LCMP);

        BranchInstruction branch =
            InstructionFactory.createBranchInstruction(sop.shortValue(), null);
        this.qi.append(branch);
        
        this.branches.add(branch);
    }

    public void append(String branch, String val)
        throws QueryLoadException,
               MalformedQueryException {

        QueryBranch qb = new QueryBranch(branch);

        String attrClass=qb.attrClass, attr=qb.attr, op=qb.op;

        if (attrClass.equals("Env")) {
            appendEnvOp(attr, op, val);
        }
        else if (attrClass.equals("Args")) {
            if (attr.equals("*")) {
                //run op against all args
                appendArgsMatch(op, val);
            }
            else {
                int idx;
                try {
                    idx = Integer.parseInt(attr);
                } catch (NumberFormatException e) {
                    String msg = "Array index '" + attr + "' is not a number";
                    throw new MalformedQueryException(msg);
                }
                appendArgsOp(idx, op, val);
            }
        }
        else if (attrClass.equals("Port")) {
            long port;
            try {
                port = Long.parseLong(val);
            } catch (NumberFormatException e) {
                String msg = "Port value '" + val + "' is not a number";
                throw new MalformedQueryException(msg);
            }
            appendProcPortOp(attr, op, port);
        }
        else if (attrClass.equals("Pid")) {
            appendPidOp(op, val);
        }
        else {
            Method method = (Method)IFMETHODS.get(attrClass);
            
            if (method == null) {
                throw unsupportedMethod(attrClass);
            }
            
            Class subtype = method.getReturnType();
            boolean isStringType = false;

            if (isSigarClass(subtype)) {
                try {
                    method = subtype.getMethod("get" + attr,
                                               NOPARAM);
                } catch (NoSuchMethodException e) {
                    throw unsupportedAttribute(attr);
                }
                
                if (method.getReturnType() == String.class) {
                    isStringType = true;
                }
                else if (method.getReturnType() == Character.TYPE) {
                    if (val.length() != 1) {
                        String msg = val + " is not a char";
                        throw new MalformedQueryException(msg);
                    }

                    int c = (int)val.charAt(0);
                    appendNumberOp(attrClass, attr, op, Type.CHAR,
                                   0, 0.0, c);
                    return;
                }
                else if (method.getReturnType() == Double.TYPE) {
                    try {
                        double doubleVal = Double.parseDouble(val);
                        appendNumberOp(attrClass, attr, op, doubleVal);
                        return;
                    } catch (NumberFormatException e) {
                        String msg = val + " is not a double";
                        throw new MalformedQueryException(msg);
                    }
                }
            }
            else {
                isStringType = true;
            }
            
            if (!isStringType && Character.isDigit(val.charAt(0))) {
                try {
                    long longVal = Long.parseLong(val);
                    appendNumberOp(attrClass, attr, op, longVal);
                    return;
                } catch (NumberFormatException e) {
                }
            }
            
            appendStringOp(attrClass, attr, op, val);
        }
    }

    String addModifier(String key, char modifier) 
        throws MalformedQueryException {

        int ix;
        if ((ix = key.lastIndexOf(".")) < 0) {
            throw new MalformedQueryException();
        }

        return key.substring(0, ix+1) + modifier +
               key.substring(ix+1, key.length());
    }

    public void finish() {
        this.qi.append(new PUSH(this.pool, 1));

        BranchInstruction gotoBranch =
            InstructionFactory.createBranchInstruction(Constants.GOTO, null);

        this.qi.append(gotoBranch);

        InstructionHandle target =
            this.qi.append(new PUSH(this.pool, 0));

        InstructionHandle retval =
            this.qi.append(InstructionFactory.createReturn(Type.INT));

        for (int i=0; i<this.branches.size(); i++) {
            BranchInstruction branch =
                (BranchInstruction)this.branches.get(i);
            branch.setTarget(target);
        }

        gotoBranch.setTarget(retval);

        this.query.setMaxStack();
        this.query.setMaxLocals();
        this.generator.addMethod(this.query.getMethod());
        this.qi.dispose();
    }

    public ProcessQuery load()
        throws QueryLoadException {

        if (dumpClasses) {
            try {
                FileOutputStream os =
                    new FileOutputStream(getClassName() + ".class");
                dump(os);
                os.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        byte[] bytecode = this.generator.getJavaClass().getBytes();
        ClassLoader parent = ClassLoader.getSystemClassLoader();
        ClassLoader cl = new ProcessQueryClassLoader(parent, bytecode);
        Class genclass;

        try {
            genclass = cl.loadClass(getClassName());
        } catch (ClassNotFoundException e) {
            throw new QueryLoadException("ClassNotFoundException: " +
                                         e.getMessage());
        }

        try {
            return (ProcessQuery)genclass.newInstance();
        } catch (InstantiationException e) {
            throw new QueryLoadException("InstantiationException: " +
                                         e.getMessage());
        } catch (IllegalAccessException e) {
            throw new QueryLoadException("IllegalAccessException: " +
                                         e.getMessage());
        }
    }

    public void dump(OutputStream os) throws IOException {
        this.generator.getJavaClass().dump(os);
    }
}
