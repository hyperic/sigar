/*
 * Copyright (C) [2004-2009], Hyperic, Inc.
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

#define UNICODE
#define _UNICODE
#define _WIN32_DCOM

#include <windows.h>
#include <objbase.h>
#include <wbemidl.h>
#include <comdef.h>
#include "sigar.h"

#pragma comment(lib, "wbemuuid.lib")

#ifndef SIGAR_CMDLINE_MAX
#define SIGAR_CMDLINE_MAX 4096
#endif

class WMI {

  public:
    WMI();
    ~WMI();
    HRESULT Open(LPCTSTR machine=NULL, LPCTSTR user=NULL, LPCTSTR pass=NULL);
    void Close();
    HRESULT GetProcStringProperty(DWORD pid, TCHAR *name, TCHAR *value, DWORD len);
    HRESULT GetProcExecutablePath(DWORD pid, TCHAR *value);
    HRESULT GetProcCommandLine(DWORD pid, TCHAR *value);

  private:
    IWbemServices *wbem;
    BSTR GetProcQuery(DWORD pid);
};

WMI::WMI()
{
    wbem = NULL;
    CoInitializeEx(NULL, COINIT_MULTITHREADED);
}

WMI::~WMI()
{
    Close();
    CoUninitialize();
}

HRESULT WMI::Open(LPCTSTR machine, LPCTSTR user, LPCTSTR pass)
{
    HRESULT result;
    IWbemLocator *locator;
    wchar_t path[MAX_PATH];

    if (wbem) {
        return S_OK;
    }

    result =
        CoInitializeSecurity(NULL,                        //Security Descriptor
                             -1,                          //COM authentication
                             NULL,                        //Authentication services
                             NULL,                        //Reserved
                             RPC_C_AUTHN_LEVEL_DEFAULT,   //Default authentication
                             RPC_C_IMP_LEVEL_IMPERSONATE, //Default Impersonation
                             NULL,                        //Authentication info
                             EOAC_NONE,                   //Additional capabilities
                             NULL);                       //Reserved

    result = CoCreateInstance(CLSID_WbemLocator,
                              NULL, /* IUnknown */
                              CLSCTX_INPROC_SERVER,
                              IID_IWbemLocator,
                              (LPVOID *)&locator);

    if (FAILED(result)) {
        return result;
    }

    if (machine == NULL) {
        machine = L".";
    }

    wsprintf(path, L"\\\\%S\\ROOT\\CIMV2", machine);

    result = locator->ConnectServer(bstr_t(path), //Object path of WMI namespace
                                    bstr_t(user), //User name. NULL = current user
                                    bstr_t(pass), //User password. NULL = current
                                    NULL,         //Locale. NULL indicates current
                                    0,            //Security flags
                                    NULL,         //Authority (e.g. Kerberos)
                                    NULL,         //Context object
                                    &wbem);       //pointer to IWbemServices proxy

    locator->Release();

    return result;
}

void WMI::Close()
{
    if (wbem) {
        wbem->Release();
        wbem = NULL;
    }
}

BSTR WMI::GetProcQuery(DWORD pid)
{
    wchar_t query[56];
    wsprintf(query, L"Win32_Process.Handle=%d", pid);
    return bstr_t(query);
}

HRESULT WMI::GetProcStringProperty(DWORD pid, TCHAR *name, TCHAR *value, DWORD len)
{
    HRESULT result;
    IWbemClassObject *obj;
    VARIANT var;

    result = wbem->GetObject(GetProcQuery(pid), 0, 0, &obj, 0);

    if (FAILED(result)) {
        return result;
    }

    result = obj->Get(name, 0, &var, 0, 0);

    if (SUCCEEDED(result)) {
        if (var.vt == VT_NULL) {
            result = E_INVALIDARG;
        }
        else {
            lstrcpyn(value, var.bstrVal, len);
        }
        VariantClear(&var);
    }

    obj->Release();

    return result;
}

HRESULT WMI::GetProcExecutablePath(DWORD pid, TCHAR *value)
{
    return GetProcStringProperty(pid, L"ExecutablePath", value, MAX_PATH);
}

HRESULT WMI::GetProcCommandLine(DWORD pid, TCHAR *value)
{
    return GetProcStringProperty(pid, L"CommandLine", value, SIGAR_CMDLINE_MAX);
}

/* in peb.c */
extern "C" int sigar_parse_proc_args(sigar_t *sigar, WCHAR *buf,
                                     sigar_proc_args_t *procargs);

extern "C" int sigar_proc_args_wmi_get(sigar_t *sigar, sigar_pid_t pid,
                                       sigar_proc_args_t *procargs)
{
    int status;
    TCHAR buf[SIGAR_CMDLINE_MAX];
    WMI *wmi = new WMI();

    if (FAILED(wmi->Open())) {
        return GetLastError();
    }

    if (FAILED(wmi->GetProcCommandLine(pid, buf))) {
        status = GetLastError();
    }
    else {
        status = sigar_parse_proc_args(sigar, buf, procargs);
    }

    wmi->Close();
    delete wmi;

    return status;
}
