'MTABBS v. 2.6 to Black Hole BBS v. 1.3 user conversion program
'by Brad Hufnagel
'----------------------------------------------------------------------------

cls
print "MTABBS v. 2.6 to Black Hole BBS v. 1.3 user conversion program"
print "--------------------------------------------------------------"
print
print "Make sure that you have the MTABBS files USER/FIL and USERAUX/FIL"
print "present in the current directory as USER.FIL and USERAUX.FIL."
print
line input "Enter the user record to convert up through ->";record$

record = val(record$)
if record < 1 then end

print
print "Hit any key to start..."
pause:
   if inkey$ = "" then pause
print

open "USER.DAT" as #1 len = 134
   field #1, 20 as username$, 20 as userhandle$, 22 as useraddress$,_
   22 as usercity$, 10 as userphone$, 8 as userpassword$, 1 as userseclevel$,_
   1 as usertimeallowed$, 1 as usertimeontoday$, 2 as usertotalcalls$,_
   3 as userfirstcall$, 5 as userlastcall$, 2 as userlastmsgptr$,_
   1 as usertype$, 1 as uservideo$, 1 as usertermstatus$, 6 as userfilter$,_
   4 as userupload$, 4 as userdownload$

open "USER.FIL" as #2 len = 64
   field #2, 20 as mtahandle$, 20 as mtaname$, 1 as mtaseclevel$,_
   5 as mtalastcall$, 1 as mtatermstatus$, 1 as mtavideo$,_
   2 as mtalastmsgptr$, 2 as mtatotalcalls$, 6 as mtafilter$, 2 as mtabitmap$,_
   1 as mtatimeallowed$, 2 as mtasyslastmsg$, 1 as mtatimeontoday$

open "USERAUX.FIL" as #3 len = 85
   field #3, 8 as mtapassword$, 2 as mtalastdigit$, 2 as mtaareacode$,_
   2 as mtafirstdigit$, 23 as mtaaddress$, 23 as mtacity$,_
   3 as mtafirstcall$, 22 as mtamisc$

userid = 0
do
   incr userid
   get #2, userid
   get #3, userid
   print "User #";
   print using "####";userid;
   print " - ";
   if asc(mtavideo$) = 0_
      then print "[Deleted User]"_
      else print mtahandle$
   if asc(mtavideo$) = 0_
      then lset username$ = chr$(1) : lset userhandle$ = chr$(1)_
      else lset username$ = mtaname$ : lset userhandle$ = mtahandle$
   lset useraddress$ = mtaaddress$
   lset usercity$ = mtacity$
   areacode$ = str$(cvi(mtaareacode$))
   areacode$ = right$(areacode$,len(areacode$) - 1)
   firstdigit$ = str$(cvi(mtafirstdigit$))
   firstdigit$ = right$(firstdigit$,len(firstdigit$) - 1)
   lastdigit$ = str$(cvi(mtalastdigit$))
   lastdigit$ = right$(lastdigit$,len(lastdigit$) - 1)
   lset userphone$ = areacode$ + firstdigit$ + lastdigit$
   lset userpassword$ = mtapassword$
   lset userseclevel$ = chr$(asc(mtaseclevel$) and 15)
   lset usertimeallowed$ = mtatimeallowed$
   lset usertimeontoday$ = mtatimeontoday$
   lset usertotalcalls$ = mtatotalcalls$
   lset userfirstcall$ = mtafirstcall$
   lset userlastcall$ = mtalastcall$
   lset userlastmsgptr$ = mki$(0)
   lset usertype$ = chr$(0)
   videowidth = asc(mtavideo$)
   if videowidth <> 0 then incr videowidth
   if videowidth > 80 then videowidth = 80
   lset uservideo$ = chr$(videowidth)
   termstatus = 32
   if (asc(mtatermstatus$) and 31) > 15_
      then termstatus = termstatus or 15_
      else termstatus = termstatus or (asc(mtatermstatus$) and 15)
   if (asc(mtatermstatus$) and 32) = 32_
      then termstatus = termstatus or 16
   lset usertermstatus$ = chr$(termstatus)
   lset userfilter$ = string$(6,0)
   lset userupload$ = mkl$(0)
   lset userdownload$ = mkl$(0)
   put #1, userid
loop until (userid = record) or eof(2)
close #2
close #1

print
print "Conversion complete."

