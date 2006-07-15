/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

package org.hyperic.sigar.shell;

import java.util.Stack;

import sun.misc.Signal;
import sun.misc.SignalHandler;

public class ShellIntHandler implements SignalHandler {

    private static ShellBase handlerShell;
    private static Stack handlers;

    public static void register(ShellBase shell) {
        handlerShell = shell;
        handlers = new Stack();

        Signal signal;
        try {
            signal = new Signal("INT");
        } catch (IllegalArgumentException e) {
            return; //e.g NetWare
        }

        try {
            Signal.handle(signal, new ShellIntHandler());
        } catch(Exception e) {
            //java -Xrs for example will throw IllegalArgumentException
        }
    }

    public void handle(Signal signal) {
        if (handlers.empty()) {
            handlerShell.shutdown();
            Runtime.getRuntime().halt(0);
        }
        else {
            SIGINT handler = (SIGINT)handlers.peek();
            handler.handleSIGINT();
        }
    }

    public static void push(SIGINT handler) {
        handlers.push(handler);
    }

    public static void pop() {
        handlers.pop();
    }
}
