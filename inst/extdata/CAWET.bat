@echo off
REM get the path of the current script
set SCRIPT_DIR=%~dp0
set SCRIPT_DIR=%SCRIPT_DIR:~0,-1%
REM Store current directory
set CURRENT_DIR=%cd%
REM Change to the script directory
cd /d "%SCRIPT_DIR%"
REM Update CAWET to the latest version
"r-portable-windows-master\bin\R.exe" -q -e "update.packages(repos = c('https://inrae.r-universe.dev', 'https://cloud.r-project.org'), ask = FALSE)"
REM Launch CAWET using the portable Rscript
"r-portable-windows-master\bin\R.exe" -q -e "CAWET::run()"
cd /d "%CURRENT_DIR%"
pause
