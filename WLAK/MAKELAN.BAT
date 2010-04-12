@echo off
if "%1"=="" goto usage
if not exist %1.lng goto lngmissing
if not exist %1.txt goto txtmissing
xvhc %1.txt /ns /nr
trc %1.lng %1.lan tmp.pas
trc i_%1.lng i_%1.lan tmp.pas
del tmp.pas > nul
del %1.pas > nul
goto halt
:lngmissing
echo %1.LNG not found
goto halt
:txtmissing
echo %1.TXT not found
goto halt
:usage
echo Usage: MAKELAN language
echo Example: MAKELAN eng230
:halt
