@echo off
if "%1"=="--help" goto documentation
if "%1"=="-h"     goto documentation
goto :parseopts

:documentation
echo Usage: %~nx0 [compile-opt*] [init-opt*] [main-opt] [arg*]
echo.
echo   -h, --help           Print this help message and exit
echo   -v                   Prints version and exits
echo   -c, --compile        Compile files
echo   --werl               Uses Erlang's Windows shell GUI
echo.
echo   compile-opt:
echo    -pa PATH            Adds PATH to the beginning of the code path
echo    -pz PATH            Adds PATH to the end of the code path
echo    -o PATH             Specify output directory for compiled files
echo    -t, --time          Print the time spent on compiling
echo    --to-core           Output the Core Erlang representation to a file
echo    --to-asm            Output the BEAM assembly representation to a file
echo    FILES               List of .clje files
echo.
echo   init-opt:
echo    -i, --init path     Load a file or resource
echo    -e, --eval string   Eval exprs in string; print non-nil values
echo.
echo   main-opt:
echo    -m, --main ns-name  Call the -main function from namespace w/args
echo    -r, --repl          Run a repl
echo    path                Run a script from a file or resource
echo    -                   Run a script from standard input
goto end

:parseopts

rem Parameters for Erlang
set erlArgs=-s clojerl_cli start +pc unicode -noshell

rem Parameters for Clojerl
set cljArgs=

rem Get the original path name from the batch file
set self=%~dp0
set cljeRoot=%self%..

rem Flag which determines whether or not to use werl vs erl
set useWerl=0

rem When no parameters are provided just start the REPL
set maybeStartREPL=
if "%1"=="" (set maybeStartREPL=-r)

rem Parse parameters
:startloop
set par=%1
shift
rem When there are no more parameters
if "%par%"=="" (goto erl_libs)
rem Process parameters
if "%par%"==""--werl"" (set "useWerl=1" && goto startloop)
rem Erlang arguments
if ""=="%par:-pa=%"  (set "erlArgs=%erlArgs% -pa %1" && shift && goto startloop)
if ""=="%par:-pz=%"  (set "erlArgs=%erlArgs% -pz %1" && shift && goto startloop)
rem Clojure arguments
set cljArgs=%cljArgs% %par%
rem Recursion
goto startloop

:erl_libs
set ERL_LIBS=%cljeRoot%\_build\default\lib;%ERL_LIBS%

:run
if %useWerl% equ 1 (
  start werl.exe %erlArgs% -extra %maybeStartREPL% %cljArgs%
) else (
  erl.exe %erlArgs% -extra %maybeStartREPL% %cljArgs%
)
:end
endlocal
