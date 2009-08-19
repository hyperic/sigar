@rem  $%BEGINLICENSE%$
@rem  $%ENDLICENSE%$
@echo "Run this from a shell started with the Visual Studio Build environment set!"

@IF DEFINED GENERATOR (GOTO GENERAL_CONF)
@rem Sane default is VS2005, but maybe not what we really want...
@SET GENERATOR="Visual Studio 8 2005"
@GOTO GENERAL_CONF

:GENERAL_CONF

@echo Using %GENERATOR%

@rem MSVC 8 2005 doesn't seem to have devenv.com
@SET VS_CMD="%VS90COMNTOOLS%\..\IDE\VCExpress.exe"

@rem clear the cache if neccesary to let cmake recheck everything
@rem del CMakeCache.txt

:CMAKE
@rem make sure that /D NDEBUG isn't set as it disables all the assert()ions in the testcase
cmake -G %GENERATOR% -DBUILD_NUMBER=%BUILD_NUMBER% -DCMAKE_INSTALL_PREFIX=%INST_PREFIX% -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_C_FLAGS_RELWITHDEBINFO:STRING="/MD /Zi /O2 /Ob1" .

@IF NOT %GENERATOR%=="NMake Makefiles" (GOTO VS08BUILD)
nmake
IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%
cp build-src/sigar.dll build-tests/
nmake test
IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL% 
nmake install
IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL% 

@GOTO CLEANUP

:VS08BUILD
%VS_CMD% mysql-proxy.sln /Clean
%VS_CMD% mysql-proxy.sln /Build Release
%VS_CMD% mysql-proxy.sln /Build Release /project RUN_TESTS
%VS_CMD% mysql-proxy.sln /Build Release /project PACKAGE
%VS_CMD% mysql-proxy.sln /Build Release /project INSTALL

@GOTO CLEANUP

@rem if you use VS8 to build then VS80COMNTOOLS should be set
@rem "%VS80COMNTOOLS%\..\IDE\devenv.com" mysql-proxy.sln /Clean
@rem "%VS80COMNTOOLS%\..\IDE\devenv.com" mysql-proxy.sln /Build
@rem "%VS80COMNTOOLS%\..\IDE\devenv.com" mysql-proxy.sln /Build Debug /project RUN_TESTS
@rem "%VS80COMNTOOLS%\..\IDE\devenv.com" mysql-proxy.sln /Build Debug /project PACKAGE
@rem "%VS80COMNTOOLS%\..\IDE\devenv.com" mysql-proxy.sln /Build Debug /project INSTALL

:CLEANUP

:END
