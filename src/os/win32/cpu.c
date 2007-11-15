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

#if defined(_M_AMD64) && (_M_AMD64 == 100)
#define NO_ASM
#endif

/*
 * code in this file derived from:
 * http://www.intel.com/cd/ids/developer/asmo-na/eng/technologies/20438.htm
 * license grants use of the source code.
 */
static unsigned int  HTSupported(void);
static unsigned char LogicalProcPerPhysicalProc(unsigned int);
static unsigned char GetAPIC_ID(unsigned int);
static unsigned char CPUCount(unsigned char *, unsigned char *);

// EDX[28]  Bit 28 is set if HT is supported
#define HT_BIT             0x10000000

// EAX[11:8] Bit 8-11 contains family processor ID.
#define FAMILY_ID          0x0F00

#define PENTIUM4_ID        0x0F00         

// EAX[23:20] Bit 20-23 contains extended family processor ID
#define EXT_FAMILY_ID      0x0F00000


// EBX[23:16] Bit 16-23 in ebx contains the number of logical
// processors per physical processor when execute cpuid with 
// eax set to 1
#define NUM_LOGICAL_BITS   0x00FF0000

// EBX[31:24] Bits 24-31 (8 bits) return the 8-bit unique 
// initial APIC ID for the processor this code is running on.
// Default value = 0xff if HT is not supported
#define INITIAL_APIC_ID_BITS  0xFF000000

// Status Flag
#define HT_NOT_CAPABLE           0
#define HT_ENABLED               1
#define HT_DISABLED              2
#define HT_SUPPORTED_NOT_ENABLED 3
#define HT_CANNOT_DETECT         4

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_os.h"

#ifndef NO_ASM
static unsigned int HTSupported(void)
{
    unsigned int
        Regedx = 0,
        Regeax = 0,
        VendorId[3] = {0, 0, 0};

    __try {
        __asm {
            xor eax, eax          // call cpuid with eax = 0
            cpuid                 // Get vendor id string
            mov VendorId, ebx
            mov VendorId + 4, edx
            mov VendorId + 8, ecx
                        
            mov eax, 1            // call cpuid with eax = 1
            cpuid
            mov Regeax, eax      // eax contains family processor type
            mov Regedx, edx      // edx has info about the availability of hyper-Threading
        }
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        return 0;                   // cpuid is unavailable
    }

    if (((Regeax & FAMILY_ID) ==  PENTIUM4_ID) || 
        (Regeax & EXT_FAMILY_ID))
    {
        if ((VendorId[0] == 'uneG') &&
            (VendorId[1] == 'Ieni') &&
            (VendorId[2] == 'letn'))
        {
            // Genuine Intel with hyper-Threading technology
            return Regedx & HT_BIT;
        }
        else {
            // Assume AMD
            return Regedx & HT_BIT;
        }
    }

    return 0;    // Not genuine Intel processor
}

static unsigned char LogicalProcPerPhysicalProc(unsigned int ht_supported)
{
    unsigned int Regebx = 0;
    if (!ht_supported) {
        return (unsigned char) 1;  // HT not supported
    }

    // Logical processor = 1
    __asm {
        mov eax, 1
        cpuid
        mov Regebx, ebx
    }

    return (unsigned char) ((Regebx & NUM_LOGICAL_BITS) >> 16);
}

static unsigned char GetAPIC_ID(unsigned int ht_supported)
{
    unsigned int Regebx = 0;
    if (!ht_supported) {
        return (unsigned char) -1;
    }

    // Logical processor = 1
    __asm {
        mov eax, 1
        cpuid
        mov Regebx, ebx
    }

    return (unsigned char) ((Regebx & INITIAL_APIC_ID_BITS) >> 24);
}
#endif /* NO_ASM */

static unsigned char CPUCount(unsigned char *LogicalNum,
                              unsigned char *PhysicalNum)
{
#ifdef NO_ASM
    unsigned int ht_supported = 0;
#else
    unsigned int ht_supported = HTSupported();
    unsigned char StatusFlag = 0;
#endif
    SYSTEM_INFO info;

    *PhysicalNum = 0;
    *LogicalNum  = 0;
    info.dwNumberOfProcessors = 0;
    GetSystemInfo(&info);

    // Number of physical processors in a non-Intel system
    // or in a 32-bit Intel system with Hyper-Threading technology disabled
    *PhysicalNum = (unsigned char) info.dwNumberOfProcessors;  

#ifdef NO_ASM
    *LogicalNum = 1;
    return HT_NOT_CAPABLE;
#else
    if (ht_supported) {
        unsigned char HT_Enabled = 0;

        *LogicalNum = LogicalProcPerPhysicalProc(ht_supported);

        if (*LogicalNum >= 1) {   // >1 Doesn't mean HT is enabled in the BIOS
            HANDLE hCurrentProcessHandle;
            DWORD  dwProcessAffinity;
            DWORD  dwSystemAffinity;
            DWORD  dwAffinityMask;

            // Calculate the appropriate  shifts and mask based on the 
            // number of logical processors.

            unsigned char i = 1,
                PHY_ID_MASK  = 0xFF,
                PHY_ID_SHIFT = 0;

            while (i < *LogicalNum) {
                i *= 2;
                PHY_ID_MASK  <<= 1;
                PHY_ID_SHIFT++;
            }
                        
            hCurrentProcessHandle = GetCurrentProcess();

            GetProcessAffinityMask(hCurrentProcessHandle,
                                   &dwProcessAffinity,
                                   &dwSystemAffinity);

            // Check if available process affinity mask is equal to the
            // available system affinity mask
            if (dwProcessAffinity != dwSystemAffinity) {
                StatusFlag = HT_CANNOT_DETECT;
                // *PhysicalNum = (unsigned char)-1;
                CloseHandle(hCurrentProcessHandle);
                return StatusFlag;
            }

            dwAffinityMask = 1;

            while (dwAffinityMask != 0 && dwAffinityMask <= dwProcessAffinity) {
                // Check if this CPU is available
                if (dwAffinityMask & dwProcessAffinity) {
                    if (SetProcessAffinityMask(hCurrentProcessHandle,
                                               dwAffinityMask))
                    {
                        unsigned char APIC_ID,
                            LOG_ID,
                            PHY_ID;

                        Sleep(0); // Give OS time to switch CPU

                        APIC_ID = GetAPIC_ID(ht_supported);
                        LOG_ID  = APIC_ID & ~PHY_ID_MASK;
                        PHY_ID  = APIC_ID >> PHY_ID_SHIFT;
 
                        if (LOG_ID != 0)  {
                            HT_Enabled = 1;
                        }
                    }
                }

                dwAffinityMask = dwAffinityMask << 1;
            }
             
            // Reset the processor affinity
            SetProcessAffinityMask(hCurrentProcessHandle, dwProcessAffinity);
            
            if (*LogicalNum == 1) { // Normal P4 : HT is disabled in hardware
                StatusFlag = HT_DISABLED;
            }
            else {
                if (HT_Enabled) {
                    // Total physical processors in a Hyper-Threading enabled system.
                    *PhysicalNum /= (*LogicalNum);
                    StatusFlag = HT_ENABLED;
                }
                else {
                    StatusFlag = HT_SUPPORTED_NOT_ENABLED;
                }
            }
            CloseHandle(hCurrentProcessHandle);
        }
    }
    else {
        // Processors do not have Hyper-Threading technology
        StatusFlag = HT_NOT_CAPABLE;
        *LogicalNum = 1;
    }

    return StatusFlag;
#endif /* NO_ASM */
}

unsigned int sigar_cpu_count(sigar_t *sigar)
{
    unsigned char
        status,
        LogicalNum = 0,
        PhysicalNum = 0;

    if (sigar->ncpu != 0) {
        return sigar->ncpu;
    }

    status = CPUCount(&LogicalNum, &PhysicalNum);

    sigar->ncpu = (unsigned int)PhysicalNum;

    if (status == HT_ENABLED) {
        sigar->ht_enabled = 1;
        sigar->lcpu = LogicalNum;
    }
    else {
        sigar->ht_enabled = 0;
        sigar->lcpu = 0;
    }

    return sigar->ncpu;
}

/* this function is not part of the intel derived code */

int sigar_cpu_info_get(sigar_t *sigar, sigar_cpu_info_t *info)
{
    HKEY key, cpu;
    int i = 0;
    TCHAR id[MAX_PATH + 1];
    LPBYTE value;
    DWORD size = 0, rc;

    RegOpenKey(HKEY_LOCAL_MACHINE,
               "HARDWARE\\DESCRIPTION\\System\\CentralProcessor", &key);

    //just lookup the first id, then assume all cpus are the same.
    rc = RegEnumKey(key, 0, id, sizeof(id)/sizeof(TCHAR));
    if (rc != ERROR_SUCCESS) {
        RegCloseKey(key);
        return rc;
    }
       
    rc = RegOpenKey(key, id, &cpu);
    if (rc != ERROR_SUCCESS) {
        RegCloseKey(key);
        return rc;
    }

    size = sizeof(info->vendor);
    if (RegQueryValueEx(cpu, "VendorIdentifier", NULL, NULL,
                        (LPVOID)&info->vendor, &size) ||
        strEQ(info->vendor, "GenuineIntel"))
    {
        SIGAR_SSTRCPY(info->vendor, "Intel");
    }
    else {
        if (strEQ(info->vendor, "AuthenticAMD")) {
            SIGAR_SSTRCPY(info->vendor, "AMD");
        }
    }

    size = sizeof(info->model);
    if (RegQueryValueEx(cpu, "ProcessorNameString", NULL, NULL,
                        (LPVOID)&info->model, &size))
    {
        size = sizeof(info->model);
        if (RegQueryValueEx(cpu, "Identifier", NULL, NULL,
                            (LPVOID)&info->model, &size))
        {
            SIGAR_SSTRCPY(info->model, "x86");
        }
    }
    else {
        sigar_cpu_model_adjust(sigar, info);
    }

    size = sizeof(info->mhz); // == sizeof(DWORD)
    if (RegQueryValueEx(cpu, "~MHz", NULL, NULL,
                        (LPVOID)&info->mhz, &size))
    {
        info->mhz = -1;
    }

    info->cache_size = -1; //XXX
    RegCloseKey(key);
    RegCloseKey(cpu);

    return SIGAR_OK;
}

#ifdef CPU_MAIN
void main(void)
{
    // Number of logical CPU per ONE PHYSICAL CPU
    unsigned char LogicalNum = 0,
        PhysicalNum  = 0,  // Total number of physical processor
        HTStatusFlag = 0;  

    sigar_cpu_info_t info;

    sigar_cpu_info_get(NULL, &info);

    printf("Vendor............%s\n", info.vendor);
    printf("Model.............%s\n", info.model);
    printf("Mhz...............%d\n", info.mhz);

    HTStatusFlag = CPUCount(&LogicalNum, &PhysicalNum);

    switch(HTStatusFlag)
    {
      case HT_NOT_CAPABLE:
        printf("Hyper-threading...not capable\n");
        break;

      case HT_DISABLED:
        printf("Hyper-threading...disabled\n");
        break;

      case HT_ENABLED:
        printf("Hyper-threading...enabled\n");
        break;

      case HT_SUPPORTED_NOT_ENABLED:
        printf("Hyper-threading...capable but not enabled\n");
        break;

      case HT_CANNOT_DETECT:
        printf("Hyper-threading...cannot be detected\n");
        break;
    }

    printf("Logical ratio.....%d/1\n",
           LogicalNum);
   
    if (PhysicalNum != (unsigned char)-1) {
        printf("Physical CPUs.....%d\n", PhysicalNum);
    }
    else {
        printf("Can't determine number of physical processors\n");
        printf("Make sure to enable ALL processors\n");
    }
}
#endif
