@echo off
cl -I..\..\..\include -I. -DWIN32 -DCPU_MAIN cpu.c /link ADVAPI32.LIB
