using System;
using System.IO;
using System.Collections;
using System.Runtime.InteropServices;

namespace Hyperic.Sigar {
    public class Sigar {
        public const int OK = 0;

        internal const int FS_NAME_LEN = 64;

        internal const string LIBSIGAR = "sigar-x86-winnt.dll";

        internal HandleRef sigar;

        [DllImport(LIBSIGAR)]
        private static extern IntPtr sigar_new();

        [DllImport(LIBSIGAR)]
        private static extern int sigar_close(IntPtr sigar);

        public Sigar() {
            IntPtr handle = sigar_new();
            this.sigar = new HandleRef(this, handle);
        }

        public Mem Mem() {
            return Hyperic.Sigar.Mem.NativeGet(this);
        }

        public Swap Swap() {
            return Hyperic.Sigar.Swap.NativeGet(this);
        }

        public Cpu Cpu() {
            return Hyperic.Sigar.Cpu.NativeGet(this);
        }

        public CpuInfo[] CpuInfoList() {
            return Hyperic.Sigar.CpuInfoList.NativeGet(this);
        }

        public FileSystem[] FileSystemList() {
            return Hyperic.Sigar.FileSystemList.NativeGet(this);
        }

        public FileSystemUsage FileSystemUsage(string dirname) {
            return Hyperic.Sigar.FileSystemUsage.NativeGet(this, dirname);
        }

        public String[] NetInterfaceList() {
            return Hyperic.Sigar.NetInterfaceList.NativeGet(this);
        }

        ~Sigar() {
            sigar_close(this.sigar.Handle);
        }

        internal static IntPtr incrementIntPtr(IntPtr ptr, int size) {
            Int32 x = (Int32)ptr;
            x += size;
            return (IntPtr)x;
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Mem {
        public readonly ulong Ram;
        public readonly ulong Total;
        public readonly ulong Used;
        public readonly ulong Free;
        public readonly ulong Shared;
        private readonly ulong NA_buffer;
        private readonly ulong NA_cached;
        private readonly ulong NA_user;

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_mem_get(IntPtr sigar, IntPtr mem);

        internal static Mem NativeGet(Sigar sigar) {
            Type type = typeof(Mem);
            //sigar_mem_t *ptr = malloc(sizeof(*ptr))
            IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(type));

            int status = sigar_mem_get(sigar.sigar.Handle, ptr);

            if (status != Sigar.OK) {
                Marshal.FreeHGlobal(ptr);
                throw new ApplicationException("mem_get");
            }

            //memcpy(ptr, this, sizeof(this))
            Mem mem = (Mem)Marshal.PtrToStructure(ptr, type);
            Marshal.FreeHGlobal(ptr);
            return mem;
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Swap {
        public readonly ulong Total;
        public readonly ulong Used;
        public readonly ulong Free;

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_swap_get(IntPtr sigar, IntPtr swap);

        internal static Swap NativeGet(Sigar sigar) {
            Type type = typeof(Swap);
            IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(type));

            int status = sigar_swap_get(sigar.sigar.Handle, ptr);

            if (status != Sigar.OK) {
                Marshal.FreeHGlobal(ptr);
                throw new ApplicationException("swap_get");
            }

            Swap swap = (Swap)Marshal.PtrToStructure(ptr, type);
            Marshal.FreeHGlobal(ptr);

            return swap;
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Cpu {
        public readonly ulong User;
        public readonly ulong Sys;
        private readonly ulong NA_Nice;
        public readonly ulong Idle;
        public readonly ulong Total;

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_cpu_get(IntPtr sigar, IntPtr cpu);

        internal static Cpu NativeGet(Sigar sigar) {
            Type type = typeof(Cpu);
            IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(type));

            int status = sigar_cpu_get(sigar.sigar.Handle, ptr);

            if (status != Sigar.OK) {
                Marshal.FreeHGlobal(ptr);
                throw new ApplicationException("cpu_get");
            }

            Cpu cpu = (Cpu)Marshal.PtrToStructure(ptr, type);
            Marshal.FreeHGlobal(ptr);

            return cpu;
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct CpuInfo {
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst=128)]
        public readonly string Vendor; //char[128]
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst=128)]
        public readonly string Model; //char[128]
        public readonly int Mhz;
        private readonly ulong CacheSize; //XXX not implemented
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct CpuInfoList {
        private readonly uint Number; //sizeof(unsigned long) == 4
        private readonly uint size;
        private readonly IntPtr data;

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_cpu_info_list_get(IntPtr sigar,
                                                          IntPtr cpu_infos);

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_cpu_info_list_destroy(IntPtr sigar,
                                                              IntPtr cpu_infos);

        internal static CpuInfo[] NativeGet(Sigar sigar) {
            Type type = typeof(CpuInfoList);
            IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(type));

            int status = sigar_cpu_info_list_get(sigar.sigar.Handle, ptr);

            if (status != Sigar.OK) {
                Marshal.FreeHGlobal(ptr);
                throw new ApplicationException("cpu_info_list_get");
            }

            CpuInfoList infosPtr =
                (CpuInfoList)Marshal.PtrToStructure(ptr, type);

            CpuInfo[] infos = new CpuInfo[infosPtr.Number];

            IntPtr eptr = infosPtr.data;
            int size = Marshal.SizeOf(infos[0]);

            for (int i=0; i<infosPtr.Number; i++) {
                infos[i] = (CpuInfo)Marshal.PtrToStructure(eptr, typeof(CpuInfo));

                //eptr += sizeof(sigar_cpu_info_t);
                eptr = Sigar.incrementIntPtr(eptr, size);
            }

            sigar_cpu_info_list_destroy(sigar.sigar.Handle, ptr);

            Marshal.FreeHGlobal(ptr);

            return infos;
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct FileSystem {
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst=Sigar.FS_NAME_LEN)]
        public readonly string DirName;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst=Sigar.FS_NAME_LEN)]
        public readonly string DevName;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst=Sigar.FS_NAME_LEN)]
        public readonly string TypeName;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst=Sigar.FS_NAME_LEN)]
        public readonly string SysTypeName;
        public readonly int Type;
        public readonly uint Flags;
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct FileSystemList {
        private readonly uint Number;
        private readonly uint size;
        private readonly IntPtr data;

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_file_system_list_get(IntPtr sigar,
                                                             IntPtr fslist);

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_file_system_list_destroy(IntPtr sigar,
                                                                 IntPtr fslist);

        internal static FileSystem[] NativeGet(Sigar sigar) {
            Type type = typeof(FileSystemList);
            IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(type));

            int status = sigar_file_system_list_get(sigar.sigar.Handle, ptr);

            if (status != Sigar.OK) {
                Marshal.FreeHGlobal(ptr);
                throw new ApplicationException("sigar_file_system_list_get");
            }

            FileSystemList fsPtr =
                (FileSystemList)Marshal.PtrToStructure(ptr, type);

            FileSystem[] fs = new FileSystem[fsPtr.Number];

            IntPtr fptr = fsPtr.data;
            int size = Marshal.SizeOf(fs[0]);

            for (int i=0; i<fsPtr.Number; i++) {
                fs[i] = (FileSystem)Marshal.PtrToStructure(fptr, typeof(FileSystem));

                //fptr += sizeof(sigar_fs_t);
                fptr = Sigar.incrementIntPtr(fptr, size);
            }

            sigar_file_system_list_destroy(sigar.sigar.Handle, ptr);

            Marshal.FreeHGlobal(ptr);

            return fs;
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct FileSystemUsage {
        public readonly ulong Total;
        public readonly ulong Free;
        public readonly ulong Avail;
        private readonly ulong NA_Files; //XXX not implemented
        private readonly ulong NA_FreeFiles;
        public readonly double UsePercent;

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_file_system_usage_get(IntPtr sigar,
                                                              string dirname,
                                                              IntPtr fsusage);

        internal static FileSystemUsage NativeGet(Sigar sigar, string dirname) {
            Type type = typeof(FileSystemUsage);
            IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(type));

            int status = sigar_file_system_usage_get(sigar.sigar.Handle,
                                                     dirname, ptr);

            if (status != Sigar.OK) {
                Marshal.FreeHGlobal(ptr);
                throw new ApplicationException("file_system_usage_get");
            }

            FileSystemUsage fsusage =
                (FileSystemUsage)Marshal.PtrToStructure(ptr, type);

            Marshal.FreeHGlobal(ptr);

            return fsusage;
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct NetInterfaceList {
        private readonly uint number;
        private readonly uint size;
        private readonly IntPtr data;

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_net_interface_list_get(IntPtr sigar,
                                                               IntPtr iflist);

        [DllImport(Sigar.LIBSIGAR)]
        private static extern int sigar_net_interface_list_destroy(IntPtr sigar,
                                                                   IntPtr iflist);

        internal static String[] NativeGet(Sigar sigar) {
            Type type = typeof(NetInterfaceList);
            IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(type));

            int status = sigar_net_interface_list_get(sigar.sigar.Handle, ptr);

            if (status != Sigar.OK) {
                Marshal.FreeHGlobal(ptr);
                throw new ApplicationException("sigar_net_interface_list_get");
            }

            NetInterfaceList ifPtr =
                (NetInterfaceList)Marshal.PtrToStructure(ptr, type);

            String[] iflist = new String[ifPtr.number];

            IntPtr iptr = ifPtr.data;

            for (int i=0; i<ifPtr.number; i++) {
                IntPtr str = Marshal.ReadIntPtr(iptr, i * IntPtr.Size);
                iflist[i] = Marshal.PtrToStringAnsi(str);
            }

            sigar_net_interface_list_destroy(sigar.sigar.Handle, ptr);

            Marshal.FreeHGlobal(ptr);

            return iflist;
        }
    }
}
