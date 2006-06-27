import org.hyperic.sigar.*;

/*

Example to show the process state for a given pid.

Compile the example:
% javac -classpath sigar-bin/lib/sigar.jar ProcessState.java

State of the java process running the example:
% java -classpath sigar-bin/lib/sigar.jar:. ProcessState
java: Running

State of the bash shell when invoking the example is running:
% java -classpath sigar-bin/lib/sigar.jar:. ProcessState $$
bash: Sleeping

State of emacs editor used to write the example:
% java -classpath sigar-bin/lib/sigar.jar:. ProcessState 2673
emacs: Suspended

See also: examples/Ps.java, examples/Top.java

*/

public class ProcessState {

    private static String getStateString(char state) {
        switch (state) {
          case ProcState.SLEEP:
            return "Sleeping";
          case ProcState.RUN:
            return "Running";
          case ProcState.STOP:
            return "Suspended";
          case ProcState.ZOMBIE:
            return "Zombie";
          case ProcState.IDLE:
            return "Idle";
          default:
            return String.valueOf(state);
        }
    }

    public static void main(String[] args)
        throws SigarException {

        String pid;
        if (args.length == 0) {
            pid = "$$"; //default to this process
        }
        else {
            pid = args[0];
        }

        Sigar sigar = new Sigar();

        ProcState procState = sigar.getProcState(pid);
        String state;

        System.out.println(procState.getName() + ": " +
                           getStateString(procState.getState()));

        sigar.close();
    }
}
