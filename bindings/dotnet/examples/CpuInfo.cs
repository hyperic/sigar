using System;
using Hyperic.Sigar;

public class CpuInfo {

    public static void Main() {
        Sigar sigar = new Sigar();

        Hyperic.Sigar.CpuInfo[] infos = 
            sigar.CpuInfoList();

        System.Console.WriteLine(infos.Length + " total CPUs..");

        foreach (Hyperic.Sigar.CpuInfo info in infos) {
            System.Console.WriteLine("Vendor........" + info.Vendor);
            System.Console.WriteLine("Model........." + info.Model);
            System.Console.WriteLine("Mhz..........." + info.Mhz);
            System.Console.WriteLine("");
        }
    }
}
