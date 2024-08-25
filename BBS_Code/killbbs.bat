cd %1
del text\misc\*.*
rd text\misc
del text\*.*
rd text
del email\*.*
rd email
del files\*.*
rd files
cd msgs
for %%a IN (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) do del board_%%a\*.*
for %%a IN (16 17 18 19 20 21 22 23 24 25 26 27) do del board_%%a\*.*
for %%a IN (28 29 30 31 32 33 34 35 36 37 38 39) do del board_%%a\*.*
for %%a IN (40 41 42 43 44 45 46 47 48) do del board_%%a\*.*
for %%a IN (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) do rd board_%%a
for %%a IN (16 17 18 19 20 21 22 23 24 25 26 27) do rd board_%%a
for %%a IN (28 29 30 31 32 33 34 35 36 37 38 39) do rd board_%%a
for %%a IN (40 41 42 43 44 45 46 47 48) do rd board_%%a 
cd ..
rd msgs
del *.*
cd \
rd %1
