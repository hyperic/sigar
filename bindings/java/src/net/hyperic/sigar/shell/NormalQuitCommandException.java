package net.hyperic.sigar.shell;

/**
 * This exception is thrown when a command wants to exit the
 * shell completely.  Typically this is only done for quit commands.
 */
public class NormalQuitCommandException extends RuntimeException {

}
