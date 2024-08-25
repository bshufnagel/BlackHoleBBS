echo off
cls
if %2==0 goto local
:remote
ctty gate2
fsed -P%2 -w%3 -T90 -t5 %1
ctty con
goto finish
:local
fsed -c -w%3 -T90 -t5 %1
:finish
