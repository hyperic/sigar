Index: src/net/sf/antcontrib/cpptasks/CUtil.java
===================================================================
RCS file: /cvsroot/ant-contrib/cpptasks/src/net/sf/antcontrib/cpptasks/CUtil.java,v
retrieving revision 1.28
diff -u -r1.28 CUtil.java
--- src/net/sf/antcontrib/cpptasks/CUtil.java	17 Mar 2006 16:38:12 -0000	1.28
+++ src/net/sf/antcontrib/cpptasks/CUtil.java	29 Apr 2006 02:35:28 -0000
@@ -438,19 +438,19 @@
     	StringBuffer buf = new StringBuffer (attrValue);
     	int quotePos;
     	
-        for (quotePos = -1; (quotePos = buf.indexOf("\"", quotePos + 1)) >= 0;) {
+        for (quotePos = -1; (quotePos = buf.toString().indexOf("\"", quotePos + 1)) >= 0;) {
         	buf.deleteCharAt(quotePos);
         	buf.insert (quotePos, "&quot;");
         	quotePos += 5;
         }
         
-        for (quotePos = -1; (quotePos = buf.indexOf("<", quotePos + 1)) >= 0;) {
+        for (quotePos = -1; (quotePos = buf.toString().indexOf("<", quotePos + 1)) >= 0;) {
         	buf.deleteCharAt(quotePos);
         	buf.insert (quotePos, "&lt;");
         	quotePos += 3;
         }
         
-        for (quotePos = -1; (quotePos = buf.indexOf(">", quotePos + 1)) >= 0;) {
+        for (quotePos = -1; (quotePos = buf.toString().indexOf(">", quotePos + 1)) >= 0;) {
         	buf.deleteCharAt(quotePos);
         	buf.insert (quotePos, "&gt;");
         	quotePos += 3;
Index: src/net/sf/antcontrib/cpptasks/CompilerEnum.java
===================================================================
RCS file: /cvsroot/ant-contrib/cpptasks/src/net/sf/antcontrib/cpptasks/CompilerEnum.java,v
retrieving revision 1.18
diff -u -r1.18 CompilerEnum.java
--- src/net/sf/antcontrib/cpptasks/CompilerEnum.java	8 Jul 2004 22:30:53 -0000	1.18
+++ src/net/sf/antcontrib/cpptasks/CompilerEnum.java	29 Apr 2006 02:35:28 -0000
@@ -25,7 +25,9 @@
 import net.sf.antcontrib.cpptasks.devstudio.DevStudioResourceCompiler;
 import net.sf.antcontrib.cpptasks.gcc.GccCCompiler;
 import net.sf.antcontrib.cpptasks.hp.aCCCompiler;
+import net.sf.antcontrib.cpptasks.hp.HPCompiler;
 import net.sf.antcontrib.cpptasks.ibm.VisualAgeCCompiler;
+import net.sf.antcontrib.cpptasks.ibm.XlcCompiler;
 import net.sf.antcontrib.cpptasks.intel.IntelLinux32CCompiler;
 import net.sf.antcontrib.cpptasks.intel.IntelLinux64CCompiler;
 import net.sf.antcontrib.cpptasks.intel.IntelWin32CCompiler;
@@ -205,6 +207,8 @@
             new ProcessorEnumValue("armcpp", ADSCCompiler.getArmCpp()),
             new ProcessorEnumValue("tcc", ADSCCompiler.getThumbCC()),
             new ProcessorEnumValue("tcpp", ADSCCompiler.getThumbCpp()),
+            new ProcessorEnumValue("hp", HPCompiler.getInstance()),
+            new ProcessorEnumValue("xlc_r", XlcCompiler.getInstance()),
             // GCC Cross Compilers
             new ProcessorEnumValue(
                     "sparc-sun-solaris2-gcc",
Index: src/net/sf/antcontrib/cpptasks/LinkerEnum.java
===================================================================
RCS file: /cvsroot/ant-contrib/cpptasks/src/net/sf/antcontrib/cpptasks/LinkerEnum.java,v
retrieving revision 1.16
diff -u -r1.16 LinkerEnum.java
--- src/net/sf/antcontrib/cpptasks/LinkerEnum.java	8 Jul 2004 22:30:53 -0000	1.16
+++ src/net/sf/antcontrib/cpptasks/LinkerEnum.java	29 Apr 2006 02:35:28 -0000
@@ -25,7 +25,9 @@
 import net.sf.antcontrib.cpptasks.gcc.GppLinker;
 import net.sf.antcontrib.cpptasks.gcc.LdLinker;
 import net.sf.antcontrib.cpptasks.hp.aCCLinker;
+import net.sf.antcontrib.cpptasks.hp.HPLinker;
 import net.sf.antcontrib.cpptasks.ibm.VisualAgeLinker;
+import net.sf.antcontrib.cpptasks.ibm.XlcLinker;
 import net.sf.antcontrib.cpptasks.intel.IntelLinux32Linker;
 import net.sf.antcontrib.cpptasks.intel.IntelLinux64Linker;
 import net.sf.antcontrib.cpptasks.intel.IntelWin32Linker;
@@ -72,6 +74,8 @@
             new ProcessorEnumValue("armcpp", ADSLinker.getInstance()),
             new ProcessorEnumValue("tcc", ADSLinker.getInstance()),
             new ProcessorEnumValue("tcpp", ADSLinker.getInstance()),
+            new ProcessorEnumValue("hp", HPLinker.getInstance()),
+            new ProcessorEnumValue("xlc_r", XlcLinker.getInstance()),
             // gcc cross compilers
             new ProcessorEnumValue(
                     "sparc-sun-solaris2-gcc",
Index: src/net/sf/antcontrib/cpptasks/SubsystemEnum.java
===================================================================
RCS file: /cvsroot/ant-contrib/cpptasks/src/net/sf/antcontrib/cpptasks/SubsystemEnum.java,v
retrieving revision 1.7
diff -u -r1.7 SubsystemEnum.java
--- src/net/sf/antcontrib/cpptasks/SubsystemEnum.java	28 Feb 2004 20:04:02 -0000	1.7
+++ src/net/sf/antcontrib/cpptasks/SubsystemEnum.java	29 Apr 2006 02:35:28 -0000
@@ -29,6 +29,6 @@
         setValue("gui");
     }
     public String[] getValues() {
-        return (String[]) values.clone();
+        return values;
     }
 }
Index: src/net/sf/antcontrib/cpptasks/gcc/GccLinker.java
===================================================================
RCS file:
/cvsroot/ant-contrib/cpptasks/src/net/sf/antcontrib/cpptasks/gcc/GccLinker.java,v
retrieving revision 1.32
diff -u -r1.32 GccLinker.java
--- src/net/sf/antcontrib/cpptasks/gcc/GccLinker.java   28 Feb 2004
20:01:15 -0000      1.32
+++ src/net/sf/antcontrib/cpptasks/gcc/GccLinker.java   29 Apr 2006
17:27:50 -0000
@@ -35,7 +35,7 @@
     private static String[] linkerOptions = new String[]{"-bundle",
             "-dynamiclib", "-nostartfiles", "-nostdlib", "-prebind",
	     "-s",
             "-static", "-shared", "-symbolic", "-Xlinker",
-            "--export-all-symbols", "-static-libgcc",};
+            "--export-all-symbols", "-static-libgcc", "-arch"};
     private static final GccLinker dllLinker = new GccLinker("gcc",
     objFiles,
             discardFiles, "lib", ".so", false, new GccLinker("gcc",
	     objFiles,
                     discardFiles, "lib", ".so", true, null));

--- /dev/null	2006-04-28 19:36:02.000000000 -0700
+++ src/net/sf/antcontrib/cpptasks/hp/HPCompiler.java	2006-04-28 17:08:22.000000000 -0700
@@ -0,0 +1,161 @@
+/*
+ * The Apache Software License, Version 1.1
+ *
+ * Copyright (c) 2001 The Ant-Contrib project.  All rights
+ * reserved.
+ *
+ * Redistribution and use in source and binary forms, with or without
+ * modification, are permitted provided that the following conditions
+ * are met:
+ *
+ * 1. Redistributions of source code must retain the above copyright
+ *    notice, this list of conditions and the following disclaimer.
+ *
+ * 2. Redistributions in binary form must reproduce the above copyright
+ *    notice, this list of conditions and the following disclaimer in
+ *    the documentation and/or other materials provided with the
+ *    distribution.
+ *
+ * 3. The end-user documentation included with the redistribution, if
+ *    any, must include the following acknowlegement:
+ *       "This product includes software developed by the
+ *        Apache Software Foundation (http://www.apache.org/)."
+ *    Alternately, this acknowlegement may appear in the software itself,
+ *    if and wherever such third-party acknowlegements normally appear.
+ *
+ * 4. The names "The Jakarta Project", "Ant", and "Apache Software
+ *    Foundation" must not be used to endorse or promote products derived
+ *    from this software without prior written permission. For written
+ *    permission, please contact apache@apache.org.
+ *
+ * 5. Products derived from this software may not be called "Apache"
+ *    nor may "Apache" appear in their names without prior written
+ *    permission of the Apache Group.
+ *
+ * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
+ * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
+ * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
+ * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
+ * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
+ * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
+ * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
+ * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
+ * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
+ * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
+ * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
+ * SUCH DAMAGE.
+ * ====================================================================
+ *
+ * This software consists of voluntary contributions made by many
+ * individuals on behalf of the Apache Software Foundation.  For more
+ * information on the Apache Software Foundation, please see
+ * <http://www.apache.org/>.
+ */
+package net.sf.antcontrib.cpptasks.hp;
+
+import java.io.File;
+import java.util.Vector;
+
+import net.sf.antcontrib.cpptasks.CUtil;
+import net.sf.antcontrib.cpptasks.compiler.LinkType;
+import net.sf.antcontrib.cpptasks.compiler.Linker;
+import net.sf.antcontrib.cpptasks.gcc.GccCompatibleCCompiler;
+import org.apache.tools.ant.types.Environment;
+
+/**
+ * Adapter for the HP compiler
+ *
+ * @author Curt Arnold
+ */
+public final class HPCompiler extends GccCompatibleCCompiler {
+
+	private String identifier;
+	private File[] includePath;
+	private static final HPCompiler instance =
+		new HPCompiler("cc", false, null);
+
+	/**
+	 * Private constructor.  Use GccCCompiler.getInstance() to get
+	 * singleton instance of this class.
+	 */
+	private HPCompiler(
+		String command,
+		boolean newEnvironment,
+		Environment env) {
+		super(command, "-help", false, null, newEnvironment, env);
+	}
+
+	public int getMaximumCommandLength() {
+		return Integer.MAX_VALUE;
+	}
+
+	/**
+	 * Gets singleton instance of this class
+	 */
+	public static HPCompiler getInstance() {
+		return instance;
+	}
+
+	public File[] getEnvironmentIncludePath() {
+		if (includePath == null) {
+			File ccLoc = CUtil.getExecutableLocation("cc");
+			if (ccLoc != null) {
+				File compilerIncludeDir =
+					new File(new File(ccLoc, "../include").getAbsolutePath());
+				if (compilerIncludeDir.exists()) {
+					includePath = new File[2];
+					includePath[0] = compilerIncludeDir;
+				}
+			}
+			if (includePath == null) {
+				includePath = new File[1];
+			}
+			includePath[includePath.length - 1] = new File("/usr/include");
+		}
+		return includePath;
+	}
+
+	public void addImpliedArgs(
+		Vector args,
+		boolean debug,
+		boolean multithreaded,
+		boolean exceptions,
+		LinkType linkType) {
+		args.addElement("-c");
+		if (debug) {
+			args.addElement("-g");
+		}
+		/*
+		if (multithreaded) {
+		  args.addElement("-mt");
+		}
+		*/
+		if (linkType.isSharedLibrary()) {
+			args.addElement("+z");
+		}
+	}
+
+	public void addWarningSwitch(Vector args, int level) {
+		switch (level) {
+			case 0 :
+				args.addElement("-w");
+				break;
+
+			case 1 :
+			case 2 :
+				args.addElement("+w");
+				break;
+				/*
+				        case 3:
+				        case 4:
+				        case 5:
+				        args.addElement("+w2");
+				        break;
+				*/
+		}
+	}
+
+	public Linker getLinker(LinkType linkType) {
+		return HPLinker.getInstance().getLinker(linkType);
+	}
+}
--- /dev/null	2006-04-28 19:36:02.000000000 -0700
+++ src/net/sf/antcontrib/cpptasks/hp/HPLinker.java	2006-04-28 17:08:22.000000000 -0700
@@ -0,0 +1,153 @@
+/*
+ * The Apache Software License, Version 1.1
+ *
+ * Copyright (c) 2001 The Ant-Contrib project.  All rights
+ * reserved.
+ *
+ * Redistribution and use in source and binary forms, with or without
+ * modification, are permitted provided that the following conditions
+ * are met:
+ *
+ * 1. Redistributions of source code must retain the above copyright
+ *    notice, this list of conditions and the following disclaimer.
+ *
+ * 2. Redistributions in binary form must reproduce the above copyright
+ *    notice, this list of conditions and the following disclaimer in
+ *    the documentation and/or other materials provided with the
+ *    distribution.
+ *
+ * 3. The end-user documentation included with the redistribution, if
+ *    any, must include the following acknowlegement:
+ *       "This product includes software developed by the
+ *        Apache Software Foundation (http://www.apache.org/)."
+ *    Alternately, this acknowlegement may appear in the software itself,
+ *    if and wherever such third-party acknowlegements normally appear.
+ *
+ * 4. The names "The Jakarta Project", "Ant", and "Apache Software
+ *    Foundation" must not be used to endorse or promote products derived
+ *    from this software without prior written permission. For written
+ *    permission, please contact apache@apache.org.
+ *
+ * 5. Products derived from this software may not be called "Apache"
+ *    nor may "Apache" appear in their names without prior written
+ *    permission of the Apache Group.
+ *
+ * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
+ * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
+ * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
+ * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
+ * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
+ * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
+ * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
+ * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
+ * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
+ * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
+ * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
+ * SUCH DAMAGE.
+ * ====================================================================
+ *
+ * This software consists of voluntary contributions made by many
+ * individuals on behalf of the Apache Software Foundation.  For more
+ * information on the Apache Software Foundation, please see
+ * <http://www.apache.org/>.
+ */
+package net.sf.antcontrib.cpptasks.hp;
+
+import java.io.File;
+import java.util.Vector;
+
+import net.sf.antcontrib.cpptasks.CUtil;
+import net.sf.antcontrib.cpptasks.compiler.LinkType;
+import net.sf.antcontrib.cpptasks.compiler.Linker;
+import net.sf.antcontrib.cpptasks.gcc.AbstractLdLinker;
+
+/**
+ * Adapter for HP linker
+ *
+ * @author Curt Arnold
+ */
+public final class HPLinker extends AbstractLdLinker
+{
+    private static final String[] objFiles = new String[]
+      { ".o", ".a", ".lib", ".dll", ".so", ".sl" };
+    private static final String[] discardFiles = new String[0];
+
+    private static final HPLinker instance =
+      new HPLinker("ld", objFiles, discardFiles, "", "");
+    private static final HPLinker dllLinker =
+      new HPLinker("ld", objFiles, discardFiles, "lib", ".sl");
+    private static final HPLinker arLinker =
+      new HPLinker("ld", objFiles, discardFiles, "", ".a");
+
+    private File[] libDirs;
+
+    private HPLinker(String command, String[] extensions,
+        String[] ignoredExtensions, String outputPrefix,
+        String outputSuffix) {
+        super(command, "-help", extensions, ignoredExtensions,
+          outputPrefix, outputSuffix,false,null);
+    }
+
+    public static HPLinker getInstance() {
+        return instance;
+    }
+
+    /**
+     * Returns library path.
+     *
+     */
+    public File[] getLibraryPath() {
+      if(libDirs == null) {
+        File CCloc = CUtil.getExecutableLocation("ld");
+        if(CCloc != null) {
+          File compilerLib = new File(
+            new File(CCloc, "../lib").getAbsolutePath());
+          if (compilerLib.exists()) {
+            libDirs = new File[2];
+            libDirs[0] = compilerLib;
+          }
+        }
+        if (libDirs == null) {
+          libDirs = new File[1];
+        }
+      }
+      libDirs[libDirs.length-1] = new File("/usr/lib");
+      return libDirs;
+    }
+
+    public void addImpliedArgs(boolean debug, LinkType linkType, Vector args) {
+/*      if(linkType.isStaticRuntime()) {
+        args.addElement("-static");
+      }
+*/      
+      if(linkType.isSharedLibrary()) {
+        args.addElement("-b");
+      }
+/*      
+      if (linkType.isStaticLibrary()) {
+        args.addElement("-Wl,-noshared");
+      }
+*/      
+    }
+
+
+    public Linker getLinker(LinkType type) {
+      if(type.isStaticLibrary()) {
+        return arLinker;
+      }
+      if(type.isSharedLibrary()) {
+        return dllLinker;
+      }
+      return instance;
+    }
+
+    public void addIncremental(boolean incremental, Vector args) {
+    /*
+      if (incremental) {
+        args.addElement("-xidlon");
+      } else {
+        args.addElement("-xidloff");
+      }
+     */
+    }
+}
--- /dev/null	2006-04-28 19:36:02.000000000 -0700
+++ src/net/sf/antcontrib/cpptasks/ibm/XlcCompiler.java	2006-04-28 17:08:22.000000000 -0700
@@ -0,0 +1,138 @@
+/*
+ * The Apache Software License, Version 1.1
+ *
+ * Copyright (c) 2001 The Ant-Contrib project.  All rights
+ * reserved.
+ *
+ * Redistribution and use in source and binary forms, with or without
+ * modification, are permitted provided that the following conditions
+ * are met:
+ *
+ * 1. Redistributions of source code must retain the above copyright
+ *    notice, this list of conditions and the following disclaimer.
+ *
+ * 2. Redistributions in binary form must reproduce the above copyright
+ *    notice, this list of conditions and the following disclaimer in
+ *    the documentation and/or other materials provided with the
+ *    distribution.
+ *
+ * 3. The end-user documentation included with the redistribution, if
+ *    any, must include the following acknowlegement:
+ *       "This product includes software developed by the
+ *        Ant-Contrib project (http://sourceforge.net/projects/ant-contrib)."
+ *    Alternately, this acknowlegement may appear in the software itself,
+ *    if and wherever such third-party acknowlegements normally appear.
+ *
+ * 4. The names "Ant-Contrib"
+ *    must not be used to endorse or promote products derived
+ *    from this software without prior written permission. For written
+ *    permission, please contact apache@apache.org.
+ *
+ * 5. Products derived from this software may not be called "Ant-Contrib"
+ *    nor may "Ant-Contrib" appear in their names without prior written
+ *    permission of the Ant-Contrib project.
+ *
+ * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
+ * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
+ * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
+ * DISCLAIMED.  IN NO EVENT SHALL THE ANT-CONTRIB PROJECT OR
+ * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
+ * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
+ * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
+ * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
+ * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
+ * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
+ * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
+ * SUCH DAMAGE.
+ * ====================================================================
+ */
+package net.sf.antcontrib.cpptasks.ibm;
+
+import java.io.File;
+import java.util.Vector;
+
+import net.sf.antcontrib.cpptasks.compiler.LinkType;
+import net.sf.antcontrib.cpptasks.compiler.Linker;
+import net.sf.antcontrib.cpptasks.gcc.GccCompatibleCCompiler;
+import org.apache.tools.ant.types.Environment;
+
+
+/**
+ * Adapter for the IBM(r) Visual Age(tm) C++ compiler for AIX(tm)
+ *
+ * @author Curt Arnold
+ */
+public final class XlcCompiler extends GccCompatibleCCompiler {
+	private String identifier;
+	private File[] includePath;
+	private static final XlcCompiler instance =
+		new XlcCompiler("xlc_r", false, null);
+
+	/**
+	 * Private constructor.  Use getInstance() to get
+	 * singleton instance of this class.
+	 */
+	private XlcCompiler(String command, boolean newEnvironment, Environment env) {
+		super(command, "-help", false, null, newEnvironment, env);
+	}
+
+    public int getMaximumCommandLength() {
+    	return Integer.MAX_VALUE;
+    }
+
+	/**
+	 * Gets singleton instance of this class
+	 */
+	public static XlcCompiler getInstance() {
+		return instance;
+	}
+
+	public void addImpliedArgs(
+		Vector args,
+		boolean debug,
+		boolean multithreaded,
+		boolean exceptions,
+		LinkType linkType) {
+		args.addElement("-c");
+		if (debug) {
+			args.addElement("-g");
+		}
+		if (linkType.isSharedLibrary()) {
+			args.addElement("-fpic");
+			args.addElement("-qmkshrobj");
+
+		}
+	}
+
+	public void addWarningSwitch(Vector args, int level) {
+		switch (level) {
+			case 0 :
+				args.addElement("-w");
+				break;
+
+			case 1 :
+				args.addElement("-qflag=s:s");
+				break;
+
+			case 2 :
+				args.addElement("-qflag=e:e");
+				break;
+
+			case 3 :
+				args.addElement("-qflag=w:w");
+				break;
+
+			case 4 :
+				args.addElement("-qflag=i:i");
+				break;
+
+			case 5 :
+				args.addElement("-qhalt=w:w");
+				break;
+		}
+	}
+
+	public Linker getLinker(LinkType linkType) {
+		return VisualAgeLinker.getInstance().getLinker(linkType);
+	}
+}
--- /dev/null	2006-04-28 19:36:02.000000000 -0700
+++ src/net/sf/antcontrib/cpptasks/ibm/XlcLinker.java	2006-04-28 17:08:22.000000000 -0700
@@ -0,0 +1,112 @@
+/*
+ * The Apache Software License, Version 1.1
+ *
+ * Copyright (c) 2001 The Ant-Contrib project.  All rights
+ * reserved.
+ *
+ * Redistribution and use in source and binary forms, with or without
+ * modification, are permitted provided that the following conditions
+ * are met:
+ *
+ * 1. Redistributions of source code must retain the above copyright
+ *    notice, this list of conditions and the following disclaimer.
+ *
+ * 2. Redistributions in binary form must reproduce the above copyright
+ *    notice, this list of conditions and the following disclaimer in
+ *    the documentation and/or other materials provided with the
+ *    distribution.
+ *
+ * 3. The end-user documentation included with the redistribution, if
+ *    any, must include the following acknowlegement:
+ *       "This product includes software developed by the
+ *        Ant-Contrib project (http://sourceforge.net/projects/ant-contrib)."
+ *    Alternately, this acknowlegement may appear in the software itself,
+ *    if and wherever such third-party acknowlegements normally appear.
+ *
+ * 4. The names "Ant-Contrib"
+ *    must not be used to endorse or promote products derived
+ *    from this software without prior written permission. For written
+ *    permission, please contact apache@apache.org.
+ *
+ * 5. Products derived from this software may not be called "Ant-Contrib"
+ *    nor may "Ant-Contrib" appear in their names without prior written
+ *    permission of the Ant-Contrib project.
+ *
+ * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
+ * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
+ * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
+ * DISCLAIMED.  IN NO EVENT SHALL THE ANT-CONTRIB PROJECT OR
+ * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
+ * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
+ * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
+ * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
+ * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
+ * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
+ * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
+ * SUCH DAMAGE.
+ * ====================================================================
+ */
+package net.sf.antcontrib.cpptasks.ibm;
+
+import java.util.Vector;
+
+import net.sf.antcontrib.cpptasks.compiler.LinkType;
+import net.sf.antcontrib.cpptasks.compiler.Linker;
+import net.sf.antcontrib.cpptasks.gcc.AbstractLdLinker;
+import net.sf.antcontrib.cpptasks.gcc.GccLibrarian;
+
+/**
+ * Adapter for IBM(r) Visual Age(tm) Linker for AIX(tm)
+ *
+ * @author Curt Arnold
+ */
+public final class XlcLinker extends AbstractLdLinker
+{
+    private static final String[] objFiles = new String[]
+      { ".o", ".a", ".lib",".dll", ".so", ".sl"};
+    private static final String[] discardFiles = new String[]
+      { };
+
+    private static final XlcLinker instance =
+      new XlcLinker("ld", objFiles, discardFiles, "", "");
+    private static final XlcLinker dllLinker =
+      new XlcLinker("ld", objFiles, discardFiles, "lib", ".so");
+
+    private XlcLinker(String command, String[] extensions,
+        String[] ignoredExtensions, String outputPrefix,
+        String outputSuffix) {
+        //
+        //  just guessing that -? might display something useful
+        //
+        super(command, "-?", extensions, ignoredExtensions,
+          outputPrefix, outputSuffix,false,null);
+    }
+
+    public static XlcLinker getInstance() {
+        return instance;
+    }
+
+    public void addImpliedArgs(boolean debug, LinkType linkType, Vector args) {
+      if(debug) {
+        //args.addElement("-g");
+      }
+      if(linkType.isSharedLibrary()) {
+        args.addElement("-bdynamic");
+        args.addElement("-G");
+        args.addElement("-bnoentry");
+        args.addElement("-bexpall");
+        args.addElement("-lc_r");
+      }
+    }
+
+
+    public Linker getLinker(LinkType type) {
+      if(type.isStaticLibrary()) {
+        return GccLibrarian.getInstance();
+      }
+      if(type.isSharedLibrary()) {
+        return dllLinker;
+      }
+      return instance;
+    }
+}
