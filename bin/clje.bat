@echo off

set cljeBindir=%~dp0

WHERE winpty >nul 2>nul
if %ERRORLEVEL% NEQ 0 (echo Please install winpty for command editing or use "clojerl.bat" instead.)
if %ERRORLEVEL% EQU 0 (winpty %cljeBindir%/clojerl.bat %*)
