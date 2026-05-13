@echo off
REM This script installs CAWET
REM
REM Usage: Run this batch file to create the cawet directory
REM and set up the necessary environment

setlocal enabledelayedexpansion

REM Use first argument as destination folder if provided, else use current directory
if "%1"=="" (
    set DEST_DIR=%cd%
) else (
    set DEST_DIR=%~1
)
set DEST_DIR=%DEST_DIR%\CAWET

call :print_section "Installation de CAWET dans le dossier : %DEST_DIR%"

mkdir %DEST_DIR%
if errorlevel 1 goto :error

call :print_section "Telechargement de R portable..."
curl -L -o "%DEST_DIR%\r-portable-windows-master.zip" https://github.com/selkamand/r-portable-windows/archive/refs/heads/master.zip
if errorlevel 1 goto :error

call :print_section "Installation de R portable..."
powershell -Command "Expand-Archive -Path '%DEST_DIR%\r-portable-windows-master.zip' -DestinationPath '%DEST_DIR%' -Force"
if errorlevel 1 goto :error

REM Remove the zip file
del %DEST_DIR%\r-portable-windows-master.zip

set R_PATH=%DEST_DIR%\r-portable-windows-master\bin\R.exe

call :print_section "Installation du package R CropWat..."
"%R_PATH%" -q -e "install.packages('CropWat', repos = c('https://inrae.r-universe.dev', 'https://cloud.r-project.org'))"
call :print_section "Installation du package R RADIS..."
"%R_PATH%" -q -e "install.packages('RADIS', repos = c('https://inrae.r-universe.dev', 'https://cloud.r-project.org'))"
call :print_section "Installation du package R CAWET..."
"%R_PATH%" -q -e "install.packages('CAWET', repos = c('https://inrae.r-universe.dev', 'https://cloud.r-project.org'))"
if errorlevel 1 goto :error

call :print_section "Finalisation de l'installation de CAWET"
set DEST_DIR_R=%DEST_DIR:\=/%
"%R_PATH%" -q -e "CAWET::setup_CAWET('%DEST_DIR_R%')"
if errorlevel 1 goto :error

call :print_section "Installation terminee"
echo CAWET est installe dans : %DEST_DIR%
echo Pour lancer CAWET, executez le script CAWET.bat dans ce dossier.
pause
exit /b 0

:error
call :print_section "ERREUR: L'installation a echoue!"
pause
exit /b 1

REM Function to print a section header
:print_section
echo(
echo ***************************************************************************
echo * %~1
echo ***************************************************************************
exit /b 0
