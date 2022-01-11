@echo off
@setlocal enableextensions
@setlocal enabledelayedexpansion
@cd /d "%~dp0"

cls
color 9e
TITLE Generador de urls API JSON del INE
REM creamos la variable de entorno directorio para que sea usada internamente
REM por el script R
set directorio=%~dp0
echo Comprobando la instalacion de R en el equipo
echo.
echo La operacion puede tardar unos segundos...

SET RKEY=
SET RPATH=
FOR /F "tokens=* skip=2" %%L IN ('reg.exe QUERY HKLM\Software\R-core\R /f * /k ^| sort') DO (
    IF NOT "%%~L"=="" SET "RKEY=%%~L"
)
IF NOT DEFINED RKEY (
  echo Fallo en la consulta de la clave de REGISTRO: HKLM\Software\R-core\R
  pause
    exit \B 1
)
FOR /F "tokens=2* skip=2" %%A IN ('REG QUERY %RKEY% /v "installPath"') DO (
      IF NOT "%%~B"=="" SET "RPATH=%%~B"
  )

IF NOT DEFINED RPATH (
    ECHO No se ha encontrado el valor del REGISTRO %RKEY%\installPath
    pause
    EXIT /B 2
)
IF NOT EXIST "%RPATH%" (
    ECHO La ruta encontrada (%RPATH%^) no existe
    pause
    EXIT /B 3
)

SET "PathR=%RPATH%\bin\"
echo %PathR%
pause

set arg1=%~1
if defined arg1 (
for /r "%arg1%" %%a in (*) do if "%%~nxa"=="Rscript.exe" set p=%%~dpnxa
) else (
for /r "%PathR%" %%a in (*) do if "%%~nxa"=="Rscript.exe" set p=%%~dpnxa
)

if defined p (
echo Rscript ^(ruta encontrada^) --^> %p%
set /a cuenta=0
goto check
) else (
echo No se ha encontrado Rscript.exe
echo.
echo Introducir su ubicacion correcta como argumento desde la linea de comandos:
echo ejecutar.bat "C:\ruta\instalacion\R\R-..."
echo.
echo Pulse una tecla para terminar la ejecucion de este programa
pause>nul
exit
)

:check
echo Comprobando si el archivo excel se ha cerrado...
echo.
REM 2>nul anula el stderr y no mostrara errores
2>nul ren "ficha.xlsm" "ficha.xlsm"
if errorlevel 1 (
set /a cuenta+=1
echo %cuenta%/10
if %cuenta% GEQ 10 goto fin
1>nul anula el stdout y no mostrara mensajes "esperando 0 segundos..."
1>nul timeout /T 1 /nobreak
goto :check
) else (
echo excel cerrado correctamente
goto :rscript
)

:rscript
echo Comienza la ejecucion del script R
"%p%" -e "source('"metadatos_excel_RDCOM.R"')
echo Ejecucion finalizada
echo.
goto abrir

:abrir
set /p reabrir= deseas volver a abrir la hoja de calculo?(S/N)
if /i %reabrir%==s (
start excel "ficha.xlsm"
) else (
echo no quiere abrir
)

:fin
endlocal
echo.
echo FIN
echo.
pause
exit
