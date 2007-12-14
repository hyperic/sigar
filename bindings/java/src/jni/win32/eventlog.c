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

#ifdef WIN32
#include "javasigar.h"
#include "win32bindings.h"

#define MAX_MSG_LENGTH   8192
#define MAX_ERROR_LENGTH 1024

#define REG_MSGFILE_ROOT "SYSTEM\\CurrentControlSet\\Services\\EventLog\\"

static void win32_set_pointer(JNIEnv *env, jobject obj, const void *ptr)
{
    jfieldID pointer_field;
    int pointer_int;
    jclass cls;

    cls = JENV->GetObjectClass(env, obj);
    
    pointer_field = JENV->GetFieldID(env, cls, "eventLogHandle", "I");
    pointer_int = (int)ptr;

    JENV->SetIntField(env, obj, pointer_field, pointer_int);
}

static HANDLE win32_get_pointer(JNIEnv *env, jobject obj)
{
    jfieldID pointer_field;
    HANDLE h;
    jclass cls;

    cls = JENV->GetObjectClass(env, obj);

    pointer_field = JENV->GetFieldID(env, cls, "eventLogHandle", "I");
    h = (HANDLE)JENV->GetIntField(env, obj, pointer_field);

    if (!h) {
        win32_throw_exception(env, "Event log not opened");
    }

    return h;
}

static int get_messagefile_dll(const char *app, char *source, char *dllfile)
{
    HKEY hk;
    char buf[MAX_MSG_LENGTH];
    DWORD type, data = sizeof(buf);
    LONG rc;

    sprintf(buf, "%s%s\\%s", REG_MSGFILE_ROOT, app, source);
    rc = RegOpenKey(HKEY_LOCAL_MACHINE, buf, &hk); 
    if (rc) {
        return rc;
    }

    rc = RegQueryValueEx(hk, "EventMessageFile", NULL, &type,
                         (UCHAR *)buf, &data);
    if (rc) {
        RegCloseKey(hk);
        return rc;
    }

    strncpy(dllfile, buf, MAX_MSG_LENGTH);
    dllfile[MAX_MSG_LENGTH-1] = '\0';

    RegCloseKey(hk);

    return ERROR_SUCCESS;
}

static int get_formatted_message(EVENTLOGRECORD *pevlr, char *dllfile,
                                 char *msg)
{
    HINSTANCE hlib;
    LPVOID msgbuf;
    char msgdll[MAX_MSG_LENGTH];
    char *insert_strs[56], *ch;
    int i, max = sizeof(insert_strs) / sizeof(char *);
    DWORD result;

    if (!ExpandEnvironmentStrings(dllfile, msgdll, MAX_PATH))
        return GetLastError();

    if (!(hlib = LoadLibraryEx(msgdll, NULL,
                               LOAD_LIBRARY_AS_DATAFILE)))
        return GetLastError();

    memset(insert_strs, '\0', sizeof(insert_strs));
    ch = (char *)((LPBYTE)pevlr + pevlr->StringOffset);
    for (i = 0; i < pevlr->NumStrings && i < max; i++) {
        insert_strs[i] = ch;
        if (ch == NULL) {
            break;
        }
        ch += strlen(ch) + 1;
    }

    result =
        FormatMessage(FORMAT_MESSAGE_FROM_HMODULE |
                      FORMAT_MESSAGE_FROM_SYSTEM |
                      FORMAT_MESSAGE_ALLOCATE_BUFFER |
                      FORMAT_MESSAGE_ARGUMENT_ARRAY,
                      hlib,
                      pevlr->EventID,
                      MAKELANGID(LANG_NEUTRAL, SUBLANG_ENGLISH_US),
                      (LPTSTR) &msgbuf,
                      MAX_MSG_LENGTH,
                      insert_strs);

    if (result) {
        strncpy(msg, msgbuf, MAX_MSG_LENGTH);
        msg[MAX_MSG_LENGTH-1] = '\0';
        result = ERROR_SUCCESS;
        LocalFree(msgbuf);
    }
    else {
        result = GetLastError();
    }

    FreeLibrary(hlib);

    return result;
}

JNIEXPORT void SIGAR_JNI(win32_EventLog_openlog)
(JNIEnv *env, jobject obj, jstring lpSourceName)
{
    HANDLE h;
    const char *name;

    name = JENV->GetStringUTFChars(env, lpSourceName, 0);

    h = OpenEventLog(NULL, name);
    if (h == NULL) {
        char buf[MAX_ERROR_LENGTH];
        DWORD lastError = GetLastError();

        sprintf(buf, "Unable to open event log: %d", lastError);
        JENV->ReleaseStringUTFChars(env, lpSourceName, name);
        win32_throw_exception(env, buf);
        return;
    }

    JENV->ReleaseStringUTFChars(env, lpSourceName, name);

    /* Save the handle for later use */
    win32_set_pointer(env, obj, h);
}

JNIEXPORT void SIGAR_JNI(win32_EventLog_close)
(JNIEnv *env, jobject obj)
{
    HANDLE h = win32_get_pointer(env, obj);

    CloseEventLog(h);

    win32_set_pointer(env, obj, NULL);
}

JNIEXPORT jint SIGAR_JNI(win32_EventLog_getNumberOfRecords)
(JNIEnv *env, jobject obj)
{
    DWORD records;
    HANDLE h = win32_get_pointer(env, obj);

    if (!GetNumberOfEventLogRecords(h, &records)) {
        win32_throw_last_error(env);
        return 0;
    }

    return records;
}

JNIEXPORT jint SIGAR_JNI(win32_EventLog_getOldestRecord)
(JNIEnv *env, jobject obj)
{
    DWORD oldest;
    HANDLE h = win32_get_pointer(env, obj);

    if (!GetOldestEventLogRecord(h, &oldest)) {
        win32_throw_last_error(env);
        return 0;
    }

    return oldest;
}

JNIEXPORT jobject SIGAR_JNI(win32_EventLog_readlog)
(JNIEnv *env, jobject obj, jstring jname, jint recordOffset)
{
    EVENTLOGRECORD *pevlr;
    BYTE buffer[8192];
    char dllfile[MAX_MSG_LENGTH];
    char msg[MAX_MSG_LENGTH];
    DWORD dwRead, dwNeeded;
    LPSTR source, machineName;
    HANDLE h;
    BOOL rv;
    jclass cls = WIN32_FIND_CLASS("EventLogRecord");
    jobject eventObj; /* Actual instance of the EventLogRecord */
    jfieldID id;
    const char *name;

    h = win32_get_pointer(env, obj);

    pevlr = (EVENTLOGRECORD *)&buffer;
    rv = ReadEventLog(h,
                      EVENTLOG_SEEK_READ | EVENTLOG_FORWARDS_READ,
                      recordOffset,
                      pevlr,
                      sizeof(buffer),
                      &dwRead,
                      &dwNeeded);
    if (!rv) {
        char buf[MAX_ERROR_LENGTH];
        DWORD lastError = GetLastError();
        
        if (lastError == ERROR_INSUFFICIENT_BUFFER) {
            /* XXX need to handle this */
            sprintf(buf, "Buffer size (%d) too small (%d needed)",
                    sizeof(buffer), dwNeeded);
        }
        else {
            sprintf(buf, "Error reading from the event log: %d", lastError);
        }
        win32_throw_exception(env, buf);
        return NULL;
    }

    eventObj = JENV->AllocObject(env, cls);

    id = JENV->GetFieldID(env, cls, "recordNumber", "J");
    JENV->SetLongField(env, eventObj, id, pevlr->RecordNumber);

    id = JENV->GetFieldID(env, cls, "timeGenerated", "J");
    JENV->SetLongField(env, eventObj, id, pevlr->TimeGenerated);

    id = JENV->GetFieldID(env, cls, "timeWritten", "J");
    JENV->SetLongField(env, eventObj, id, pevlr->TimeWritten);

    id = JENV->GetFieldID(env, cls, "eventId", "J");
    JENV->SetLongField(env, eventObj, id, pevlr->EventID);

    id = JENV->GetFieldID(env, cls, "eventType", "S");
    JENV->SetShortField(env, eventObj, id, pevlr->EventType);

    /* Extract string data from the end of the structure.  Lame. */

    id = JENV->GetFieldID(env, cls, "source", "Ljava/lang/String;");
    source = (LPSTR)((LPBYTE)pevlr + sizeof(EVENTLOGRECORD));
    SetStringField(env, eventObj, id, source);

    name = JENV->GetStringUTFChars(env, jname, 0);

    /* Get the formatted message */
    if ((pevlr->NumStrings > 0) &&
        (get_messagefile_dll(name, source, dllfile) == ERROR_SUCCESS) &&
        (get_formatted_message(pevlr, dllfile, msg) == ERROR_SUCCESS))
    {
        id = JENV->GetFieldID(env, cls, "message", 
                              "Ljava/lang/String;");
        SetStringField(env, eventObj, id, msg);
    }
    else if (pevlr->NumStrings > 0) {
        char *tmp = (LPSTR)((LPBYTE)pevlr + pevlr->StringOffset);            
        id = JENV->GetFieldID(env, cls, "message", "Ljava/lang/String;");
        SetStringField(env, eventObj, id, tmp);
    }
    JENV->ReleaseStringUTFChars(env, jname, name);

    /* Increment up to the machine name. */
    id = JENV->GetFieldID(env, cls, "computerName", "Ljava/lang/String;");
    machineName = (LPSTR)((LPBYTE)pevlr + sizeof(EVENTLOGRECORD) +
                          strlen(source) + 1);
    SetStringField(env, eventObj, id, machineName);

    /* Get user id info */
    if (pevlr->UserSidLength > 0) {
        char name[256];
        char domain[256];
        DWORD namelen = sizeof(name);
        DWORD domainlen = sizeof(domain);
        DWORD len;
        SID_NAME_USE snu;
        PSID sid;
        
        sid = (PSID)((LPBYTE)pevlr + pevlr->UserSidOffset);
        if (LookupAccountSid(NULL, sid, name, &namelen, domain,
                             &domainlen, &snu)) {
            id = JENV->GetFieldID(env, cls, "user", "Ljava/lang/String;");
            SetStringField(env, eventObj, id, name);
        }
    }
    
    return eventObj;
}

JNIEXPORT void SIGAR_JNI(win32_EventLog_waitForChange)
(JNIEnv *env, jobject obj, jint timeout)
{
    HANDLE h, hEvent;
    DWORD millis;

    h = win32_get_pointer(env, obj);

    hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (hEvent == NULL) {
        win32_throw_exception(env, "Unable to create event");
        return;
    }

    if (timeout == -1)
        millis = INFINITE;
    else
        millis = timeout;

    if(!(NotifyChangeEventLog(h, hEvent))) {
        char buf[MAX_ERROR_LENGTH];
        sprintf(buf, "Error registering for event log to change: %d",
                GetLastError());
        win32_throw_exception(env, buf);
        return;
    }

    if (WaitForSingleObject(hEvent, millis) == WAIT_FAILED)
    {
        char buf[MAX_ERROR_LENGTH];
        sprintf(buf, "Error waiting for event log change: %d",
                GetLastError());
        win32_throw_exception(env, buf);
    }

    return;
}
#endif /* WIN32 */
