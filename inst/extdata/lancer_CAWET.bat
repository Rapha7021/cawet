@echo off
setlocal enabledelayedexpansion

echo ============================================================
echo  CAWET - Lancement
echo ============================================================
echo.

set /p CAWET_DOSSIER_TRAVAIL="Dossier de travail (ex: E:\Cawet_FL) : "

echo.
echo Nom de l'ordonnanceur [defaut : Ordonnanceur_exemple.xlsx]
set /p CAWET_NOM_ORDONNANCEUR="Appuyez sur Entree pour le defaut ou tapez le nom : "
if "!CAWET_NOM_ORDONNANCEUR!"=="" set CAWET_NOM_ORDONNANCEUR=Ordonnanceur_exemple.xlsx

echo.
echo  Dossier de travail : !CAWET_DOSSIER_TRAVAIL!
echo  Ordonnanceur       : !CAWET_NOM_ORDONNANCEUR!
echo.

REM --- Recherche de R ---
set R_EXEC=
for /f "tokens=*" %%i in ('where R.exe 2^>nul') do (
    if "!R_EXEC!"=="" set R_EXEC=%%i
)
if "!R_EXEC!"=="" (
    for /d %%i in ("C:\Program Files\R\R-*") do (
        if exist "%%i\bin\R.exe" set R_EXEC=%%i\bin\R.exe
    )
)
if "!R_EXEC!"=="" (
    for /d %%i in ("C:\Program Files (x86)\R\R-*") do (
        if exist "%%i\bin\R.exe" set R_EXEC=%%i\bin\R.exe
    )
)

if "!R_EXEC!"=="" (
    echo ERREUR : R n'est pas trouve sur ce systeme.
    echo Installez R depuis : https://cran.r-project.org
    echo.
    pause
    exit /b 1
)

echo Utilisation de R : !R_EXEC!
echo.

REM --- Conversion backslash -> slash pour R ---
set SCRIPT_PATH=%~dp0lancer_cawet.R
set SCRIPT_PATH=!SCRIPT_PATH:\=/!

"!R_EXEC!" -q -e "source('!SCRIPT_PATH!')"

echo.
pause
