using System;
using Hyperic.Sigar;

public class Free {

    public static void Main() {
        Sigar sigar = new Sigar();

	Mem mem = sigar.Mem();
	Swap swap = sigar.Swap();

        System.Console.WriteLine("\tTotal\tUsed\tFree");

        System.Console.WriteLine("Mem:\t" +
                                 mem.Total / 1024 + "\t" +
                                 mem.Used / 1024 + "\t" +
                                 mem.Free / 1024);

        System.Console.WriteLine("Swap:\t" +
                                 swap.Total / 1024 + "\t" +
                                 swap.Used / 1024 + "\t" +
                                 swap.Free / 1024);

        System.Console.WriteLine("RAM:\t" + mem.Ram + "MB");
    }
}
