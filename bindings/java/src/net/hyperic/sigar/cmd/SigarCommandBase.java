package net.hyperic.sigar.cmd;

import java.io.PrintStream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.hyperic.sigar.Sigar;
import net.hyperic.sigar.SigarProxy;
import net.hyperic.sigar.SigarException;

import net.hyperic.sigar.pager.PageControl;
import net.hyperic.sigar.pager.PageFetchException;
import net.hyperic.sigar.pager.StaticPageFetcher;

import net.hyperic.sigar.util.GetlineCompleter;
import net.hyperic.sigar.util.PrintfFormat;

import net.hyperic.sigar.shell.CollectionCompleter;
import net.hyperic.sigar.shell.ShellCommandBase;
import net.hyperic.sigar.shell.ShellCommandExecException;
import net.hyperic.sigar.shell.ShellCommandUsageException;

public abstract class SigarCommandBase
    extends ShellCommandBase
    implements GetlineCompleter {

    protected Shell shell;
    protected PrintStream out = System.out;
    protected PrintStream err = System.err;
    protected Sigar sigar;
    protected SigarProxy proxy;
    protected List output = new ArrayList();
    private CollectionCompleter completer;
    private Collection completions = new ArrayList();
    private PrintfFormat formatter;

    public SigarCommandBase(Shell shell) {
        this.shell = shell;
        this.out   = shell.getOutStream();
        this.err   = shell.getErrStream();
        this.sigar = shell.getSigar();
        this.proxy = shell.getSigarProxy();

        
        //provide simple way for handlers to implement tab completion
        this.completer = new CollectionCompleter(shell);
    }

    public SigarCommandBase() {
        this(new Shell());
        this.shell.setPageSize(PageControl.SIZE_UNLIMITED);
    }

    public void setOutputFormat(String format) {
        this.formatter = new PrintfFormat(format);
    }

    public PrintfFormat getFormatter() {
        return this.formatter;
    }

    public void printf(Object[] items) {
        println(getFormatter().sprintf(items));
    }

    public void printf(List items) {
        printf((Object[])items.toArray(new Object[0]));
    }

    public void println(String line) {
        if (this.shell.isInteractive()) {
            this.output.add(line);
        }
        else {
            this.out.println(line);
        }
    }

    public void flush() {
        try {
            this.shell.performPaging(new StaticPageFetcher(this.output));
        } catch(PageFetchException e) {
            this.err.println("Error paging: " + e.getMessage());
        } finally {
            this.output.clear();
        }
    }

    public abstract void output(String[] args)
        throws SigarException;

    protected boolean validateArgs(String[] args) {
        return args.length == 0;
    }

    public void processCommand(String[] args) 
        throws ShellCommandUsageException, ShellCommandExecException 
    {
        if (!validateArgs(args)) {
            throw new ShellCommandUsageException(getSyntax());
        }

        try {
            output(args);
        } catch (SigarException e) {
            throw new ShellCommandExecException(e.getMessage());
        }
    }

    public Collection getCompletions() {
        return this.completions;
    }

    public GetlineCompleter getCompleter() {
        return null;
    }

    public boolean isPidCompleter() {
        return false;
    }

    public String completePid(String line) {
        if ((line.length() >= 1) &&
            Character.isDigit(line.charAt(0)))
        {
            return line;
        }

        return
            ((GetlineCompleter)this.shell.getHandler("ptql")).complete(line);
    }

    public String complete(String line) {
        if (isPidCompleter()) {
            return completePid(line);
        }
        GetlineCompleter c = getCompleter();
        if (c != null) {
            return c.complete(line);
        }

        this.completer.setCollection(getCompletions());
        return this.completer.complete(line);
    }
}
