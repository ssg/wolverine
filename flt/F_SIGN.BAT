:F_SIGN
:Wolverine filter example
:Author: SSG
:(appends signature to end of messages)
:(note: Signature must be a text file named SIGN.TXT)

@echo off

if "%2"=="" goto error

copy %1+sign.txt %2
goto halt

:error
echo This filter requires Wolverine

:halt
