/*
 * Copyright (c) 2009 SpringSource, Inc.
 * Copyright (c) 2009 VMware, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#define UNICODE
#define _UNICODE
#define _WIN32_DCOM

#include <windows.h>
#include <unistd.h>
#include <stdio.h>
#include <ole2.h>
#include <initguid.h>
#include <wbemcli.h>
#include <shobjidl.h>
#include <tchar.h>
#include "sigar.h"
#include "sigar_private.h"
#include "sigar_pdh.h"
#include "sigar_os.h"
#include "sigar_util.h"
#include "sigar_format.h"

#pragma comment(lib, "wbemuuid.lib")

#ifndef SIGAR_CMDLINE_MAX
#define SIGAR_CMDLINE_MAX 4096<<2
#endif

#ifndef _MSC_VER

DEFINE_GUID(CLSID_WbemLocator, 0x4590f811, 0x1d3a, 0x11d0, 0x89, 0x1f, 0x00, 0xaa, 0x00, 0x4b, 0x2e, 0x24);
template <> const GUID & __mingw_uuidof < IWbemLocator > () {
	return IID_IWbemLocator;
}

template <> const GUID & __mingw_uuidof < IWbemLocator * >() {
	return __mingw_uuidof < IWbemLocator > ();
}
#endif
extern "C" {

int wmi_map_sigar_error(HRESULT hres)
{
	switch (hres) {
	case S_OK:
		return ERROR_SUCCESS;
	case WBEM_E_NOT_FOUND:
		return ERROR_NOT_FOUND;
	case WBEM_E_ACCESS_DENIED:
		return ERROR_ACCESS_DENIED;
	case WBEM_E_NOT_SUPPORTED:
		return SIGAR_ENOTIMPL;
	default:
		return ERROR_INVALID_FUNCTION;
	}
}

void wmi_handle_close(sigar_wmi_handle_t *wmi_handle)
{
	if(wmi_handle == NULL)
		return;

	if (wmi_handle->services) {
		wmi_handle->services->Release();
		wmi_handle->services = NULL;
	}

	if (wmi_handle->locator) {
		wmi_handle->locator->Release();
		wmi_handle->services = NULL;
	}
}

sigar_wmi_handle_t * wmi_handle_open(int *error)
{
	*error = SIGAR_OK;

	sigar_wmi_handle_t *handle;

	handle = (sigar_wmi_handle_t *)calloc(1, sizeof (*handle));

	HRESULT hres;
	wchar_t root[] = L"root\\CIMV2";

	hres = CoInitializeEx(NULL, COINIT_MULTITHREADED);
	if (FAILED(hres)) {
		goto err;
	}

	hres = CoInitializeSecurity(NULL, -1, NULL, NULL, RPC_C_AUTHN_LEVEL_CONNECT,
                                    RPC_C_IMP_LEVEL_IMPERSONATE, NULL, EOAC_NONE, 0);
	if (FAILED(hres)) {
		goto err;
	}

	hres = CoCreateInstance(CLSID_WbemLocator, NULL, CLSCTX_ALL, IID_PPV_ARGS(&handle->locator));
	if (FAILED(hres)) {
		goto err;
	}

	hres = handle->locator->ConnectServer(root, NULL, NULL, NULL,
                                              WBEM_FLAG_CONNECT_USE_MAX_WAIT, NULL, NULL, &handle->services);
	if (FAILED(hres)) {
		goto err;
	}

	return handle;
err:
	wmi_handle_close(handle);
	*error = wmi_map_sigar_error(hres);
	return NULL;
}

HRESULT wmi_get_proc_string_property(sigar_t *sigar, DWORD pid, TCHAR * name, TCHAR * value, DWORD len)
{
	IWbemClassObject *obj;
	VARIANT var;
	HRESULT result; 

	if(sigar->wmi_handle == NULL)
            return (E_INVALIDARG);

	wchar_t query[56];
	wsprintf(query, L"Win32_Process.Handle=%d", pid);

        result = sigar->wmi_handle->services->GetObject(query, 0, 0, &obj, 0);

	if (FAILED(result)) {
		return result;
	}

	result = obj->Get(name, 0, &var, 0, 0);

	if (SUCCEEDED(result)) {
		if (var.vt == VT_NULL) {
			result = E_INVALIDARG;
		} else {
			lstrcpyn(value, var.bstrVal, len);
		}
		VariantClear(&var);
	}

	obj->Release();

	return result;
}

HRESULT wmi_get_proc_executable_path(sigar_t *sigar, DWORD pid, TCHAR * value)
{
	wchar_t prop[] = L"ExecutablePath";
	return wmi_get_proc_string_property(sigar, pid, prop, value, MAX_PATH);
}

HRESULT wmi_get_proc_command_line(sigar_t *sigar, DWORD pid, TCHAR * value)
{
	wchar_t prop[] = L"CommandLine";
	return wmi_get_proc_string_property(sigar, pid, prop, value, MAX_PATH);
}

IEnumWbemClassObject *wmi_query(sigar_t *sigar, const wchar_t * query)
{
	IEnumWbemClassObject *wmi_enum = NULL;
	if (sigar->wmi_handle) {
		wchar_t lang[] = L"WQL";
		wchar_t *query_cpy = wcsdup(query);

		HRESULT hres =
			sigar->wmi_handle->services->ExecQuery(lang, query_cpy, WBEM_FLAG_FORWARD_ONLY, NULL, &wmi_enum);

		free(query_cpy);
	}
	return wmi_enum;
}

int wmi_query_sum_u64(sigar_t  *sigar, const wchar_t * query, const wchar_t * attrib,
					  sigar_uint64_t * sum, unsigned long *num_elems)
{
	*sum = 0;
	*num_elems = 0;

	IEnumWbemClassObject *wmi_enum = wmi_query(sigar, query);
	if (!wmi_enum) {
		return -1;
	}

	wchar_t *attrib_cpy = wcsdup(attrib);

	IWbemClassObject *wmi_obj = NULL;
	HRESULT hres;
	unsigned long curr_elem = 0;
	while (((hres = wmi_enum->Next(WBEM_INFINITE, 1, &wmi_obj, &curr_elem)) != WBEM_S_FALSE)
		   && !FAILED(hres)) {
		(*num_elems)++;
		VARIANT val;
		VariantInit(&val);
		if (SUCCEEDED(wmi_obj->Get(attrib_cpy, 0, &val, NULL, NULL))) {
			if (val.vt == VT_BSTR) {
				*sum += _wtoi(val.bstrVal);
			} else {
				*sum += val.intVal;
			}
		}
		wmi_obj->Release();
	}

	free(attrib_cpy);
	wmi_enum->Release();

	return 0;
}

int wmi_query_sum_u32(sigar_t *sigar, const wchar_t * query,
		const wchar_t * attrib, sigar_uint32_t * sum, unsigned long *num_elems)
{
	sigar_uint64_t sum64 = 0;
	int rc = wmi_query_sum_u64(sigar, query, attrib, &sum64, num_elems);
	*sum = sum64;
	return rc;
}

int wmi_query_avg(sigar_t *sigar, const wchar_t * query,
		const wchar_t * attrib, float *avg)
{
	sigar_uint64_t sum = 0;
	unsigned long num_elems = 0;
	int rc = wmi_query_sum_u64(sigar, query, attrib, &sum, &num_elems);
	if (!rc && num_elems) {
		*avg = sum / (double)(num_elems);
	}
	return rc;
}

/* in peb.c */
int sigar_parse_proc_args(sigar_t * sigar, WCHAR * buf, sigar_proc_args_t * procargs);

int sigar_proc_args_wmi_get(sigar_t * sigar, sigar_pid_t pid, sigar_proc_args_t * procargs)
{
	TCHAR buf[SIGAR_CMDLINE_MAX];
	int status;

	if ((status = wmi_get_proc_command_line(sigar, pid, buf))) {
		goto out;
	} else {
		status = sigar_parse_proc_args(sigar, buf, procargs);
	}

out:
	return status;
}

int sigar_proc_exe_wmi_get(sigar_t * sigar, sigar_pid_t pid, sigar_proc_exe_t * procexe)
{
	TCHAR buf[MAX_PATH + 1];
	int status;

	procexe->name[0] = '\0';

	if ((status = wmi_get_proc_executable_path(sigar, pid, buf))) {
		goto out;
	} else {
		status = SIGAR_OK;
		/* SIGAR_W2A(buf, procexe->name, sizeof(procexe->name)); */
		WideCharToMultiByte(CP_ACP, 0, buf, -1,
			(LPSTR) procexe->name, sizeof(procexe->name), NULL, NULL);
	}

out:
	return status;
}
}								//extern "C"
