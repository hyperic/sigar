using System;
using Hyperic.Sigar;

public class Df {

    private static String FormatSize(long value) {
        return Sigar.FormatSize(value * 1024);
    }

    public static void Main() {
        Sigar sigar = new Sigar();

        foreach (FileSystem fs in sigar.FileSystemList()) {
            FileSystemUsage usage;
            long used, avail, total, pct;

            try {
                usage = sigar.FileSystemUsage(fs.DirName);

                used = usage.Total - usage.Free;
                avail = usage.Avail;
                total = usage.Total;
                pct = (long)(usage.UsePercent * 100);
            } catch (SigarException) {
                used = avail = total = pct = 0;
                continue;
            }

            string usePct;
            if (pct == 0) {
                usePct = "-";
            }
            else {
                usePct = pct + "%";
            }

            System.Console.WriteLine(fs.DevName + "\t" +
                                     FormatSize(total) + "\t" +
                                     FormatSize(used) + "\t" + 
                                     FormatSize(avail) + "\t" + 
                                     usePct + "\t" +
                                     fs.DirName + "\t" + 
                                     fs.SysTypeName + "/" + fs.TypeName);
        }
    }
}
