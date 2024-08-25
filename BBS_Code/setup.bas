' Black Hole BBS v. 1.0 setup program
' by Brad Hufnagel
'----------------------------------------------------------------------------

cls
locate 1,26 : print "Black Hole BBS Setup Program"
locate 2,26 : print "----------------------------"

print
print "WARNING:  This program will reinitialize all pointer files to 0!"
print
input "Do you want to continue? (Y/N) ";a$
if ucase$(a$) <> "Y" then end

boardname:
   print
   print
   line input "Enter the name for your board ->";bbsname$
   if bbsname$ = ""_
      then print :_
           print "BBS setup terminated." :_
           end
comport:
   print
   line input "Which COM port will be used (1-2) ->";comm$
   if comm$ <> "1" and comm$ <> "2" then comport

baudrate:
   print
   print "What is the maximum baud rate of your modem?"
   line input "(1) 300 baud  (2) 1200 baud  (3) 2400 baud ->";baud$
   if baud$ <> "1" and baud$ <> "2" and baud$ <> "3" then baudrate

cls
print "Now creating system files..."

open "BH.CNF" for output as #1
   print #1, "NAME=";bbsname$
   print #1, "COM=";comm$
   print #1, "BAUD=";
   select case baud$
      case = "1"
         print #1, "300"
      case = "2"
         print #1, "1200"
      case = "3"
         print #1, "2400"
   end select
close #1

open "BHPTRS.DAT" for output as #1
   print #1, 0, 0, 0, 0, 90, 0, 15, 0, 0, 60, 100, 30, 300
   for counter = 1 to 8
      write #1, "", 0
   next counter
   print #1, 0
   for counter = 1 to 48
      write #1, "", 0, 0, 0, 0, 0
   next counter
   print #1, 0
   for counter = 1 to 48
      write #1, "", 0, 0, 0, 0
   next counter
close #1

open "SUBBOARD.IDX" as #1 len = 512
   field #1, 512 as temp$
   for counter = 1 to 48
      lset temp$ = string$(512,0)
      put #1, counter
   next counter
close #1

open "EMAIL.IDX" as #1 len = 1024
   field #1, 1024 as temp$
   lset temp$ = string$(1024,0)
   put #1, 1
close #1

open "FILEAREA.IDX" as #1 len = 1024
   field #1, 1024 as temp$
   for counter = 1 to 48
      lset temp$ = string$(1024,0)
      put #1, counter
   next counter
close #1

print
print bbsname$;" configuration complete."
end
