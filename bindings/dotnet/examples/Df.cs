using System;
using Hyperic.Sigar;

public class Df {

    public static void Main() {
        Sigar sigar = new Sigar();

        foreach (FileSystem fs in sigar.FileSystemList()) {
            FileSystemUsage usage;
            ulong used, avail, total, pct;

            try {
                usage = sigar.FileSystemUsage(fs.DirName);

                used = usage.Total - usage.Free;
                avail = usage.Avail;
                total = usage.Total;
                pct = (ulong)(usage.UsePercent * 100);
            } catch (ApplicationException) {
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
                                     total + "\t" +
                                     used + "\t" + 
                                     avail + "\t" + 
                                     usePct + "\t" +
                                     fs.DirName + "\t" + 
                                     fs.SysTypeName + "/" + fs.TypeName);
        }
    }
}
