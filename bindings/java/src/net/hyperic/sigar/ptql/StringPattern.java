package net.hyperic.sigar.ptql;

import java.lang.reflect.Method;
import java.util.Map;

import net.hyperic.sigar.util.ReferenceMap;

public class StringPattern {
    static Class patternClass;
    static Method compile, matcher, find;
    private static Map patterns = null;
    
    //XXX avoiding 1.4 requirement
    private static void init() throws Exception {
        patterns = ReferenceMap.synchronizedMap();
        patternClass = Class.forName("java.util.regex.Pattern");
        Class matcherClass = Class.forName("java.util.regex.Matcher");
        Class[] arg = { String.class };
        compile = patternClass.getDeclaredMethod("compile", arg);
        arg = new Class[] { Class.forName("java.lang.CharSequence") };
        matcher = patternClass.getDeclaredMethod("matcher", arg);
        find = matcherClass.getDeclaredMethod("find", new Class[0]);
    }

    /**
     * Wrapper around Pattern.compile(regex).matcher(source).find()
     */
    public static boolean matches(String source, String regex) {
        try {
            if (patterns == null) {
                init();
            }

            Object pattern = patterns.get(regex);
            if (pattern == null) {
                //pattern = Pattern.compile(regex)
                pattern = compile.invoke(patternClass, new Object[] { regex });
                patterns.put(regex, pattern);
            }
            //m = pattern.matcher(source)
            Object m = matcher.invoke(pattern, new Object[] { source });
            //result = m.find()
            Boolean result = (Boolean)find.invoke(m, new Object[0]);
            return result.booleanValue();
        } catch (Exception e) {
            System.err.println("Error matching '" +
                               regex + "' against '" +
                               source + "'");
            e.printStackTrace();
            return false;
        }
    }
}

