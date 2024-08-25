'Black Hole BBS main program
'by Brad Hufnagel
'----------------------------------------------------------------------------

defint a-z
on error goto bbserror

$include "subs.bas"
$include "editor.bas"

'initialize variables

initvariables:

abortable = 0 : bell = 0 : chat = 0 : comport = 1 : carrier = 0 : dump = 0
maxbaud = 300 : pause = 1 : ring = 0 : remote = 0 : cursorx = 0 : cursory = 0
bye = 0 : systype = 0 : bbspfx$ = "" : modeminit$ = "" : boot$ = "N"
dim arealist(48), areaname$(48), areaowner(48), areaul(48), areadl(48),_
    areaptr(48), boardlist(48), boardname$(48), boardread(48), boardpost(48),_
    boardowner(48), boardlimit(48), email(255), message$(51), type(8),_
    typename$(8), dummy(50)

titlepage:
   call printout(chr$(13))
   call printout("The Black Hole Bulletin Board System, version 1.3" +_
      chr$(13))
   call printout("(c) 1987-89 Brad Hufnagel, all rights reserved." + chr$(13))

initialization:
   filenm$ = bbspfx$ + "BH.CNF"
   open filenm$ for input as #4
   while not eof(4)
      input #4, env$
      test$ = ucase$(env$)
      if left$(test$,5) = "NAME=" then bbsname$ = right$(env$,len(env$) - 5)
      if left$(test$,4) = "COM=" then comport = val(right$(env$,1))
      if left$(test$,5) = "BAUD=" then maxbaud=val(right$(env$,len(env$) - 5))
      if left$(test$,6) = "MODEM=" then modeminit$ = right$(env$,len(env$) - 6)
      if left$(test$,10) = "ERRORBOOT=" then errorboot$ = right$(test$,1)
      if left$(test$,7) = "BBSDIR="_
         then bbspfx$ = right$(test$,len(test$) - 7) + "\"
   wend
   close #4

   call printout(chr$(13) + "Checking for fossil driver..." + chr$(13))
   if fninitfossil(comport) = 0_
      then call printout(chr$(13) + "Error!  Fossil driver not found." +_
              chr$(13)) :_
           end

   filenm$ = bbspfx$ + "BHPTRS.DAT"
   open filenm$ for input as #4
      input #4, totalusers, callernum, msgptr, closed, timeout, uldlratio,_
         dfltunvaltime, dfltunvallevel, dfltunvaltype, dfltvaltime,_
         msglimit, emaillimit, filelimit
      for counter = 1 to 8
         input #4, typename$(counter), type(counter)
         if type(counter) = 1 then systype = systype + (2 ^ (counter - 1))
      next counter
      input #4, totalsubboards
      for counter = 1 to 48
         input #4, boardname$(counter), boardowner(counter),_
            boardread(counter), boardpost(counter), boardptr(counter),_
            boardlimit(counter)
      next counter
      input #4, totalareas
      for counter = 1 to 48
         input #4, areaname$(counter), areaowner(counter),_
            areaul(counter), areadl(counter), areaptr(counter)
      next counter
   close #4

   filenm$ = bbspfx$ + "USER.DAT"
   open filenm$ as #2 len = 134
      field #2, 20 as username$, 20 as userhandle$, 22 as useraddress$,_
      22 as usercity$, 10 as userphone$, 8 as userpassword$,_
      1 as userseclevel$, 1 as usertimeallowed$, 1 as usertimeontoday$,_
      2 as usertotalcalls$, 3 as userfirstcall$, 5 as userlastcall$,_
      2 as userlastmsgptr$, 1 as usertype$, 1 as uservideo$,_
      1 as usertermstatus$, 6 as userfilter$, 4 as userupload$,_
      4 as userdownload$

   filenm$ = bbspfx$ + "FILES.DES"
   open filenm$ as #5 len = 118
      field #5, 12 as fdname$, 2 as fdsize$, 25 as fddate$, 20 as fdhandle$,_
      55 as fddesc$, 2 as fdtotal$, 2 as fdptr$

   filenm$ = bbspfx$ + "SUBBOARD.IDX"
   open filenm$ as #6 len = 2
      field #6, 2 as msgnum$

   filenm$ = bbspfx$ + "EMAIL.IDX"
   open filenm$ as #7 len = 4
      field #7, 2 as emailnum$, 2 as emailrecv$

   filenm$ = bbspfx$ + "FILEAREA.IDX"
   open filenm$ as #8 len = 2
      field #8, 2 as filenum$

   filenm$ = bbspfx$ + "DUMP.TXT"
   open filenm$ for append as #9

   call addhistory(fnhistdate$ + " " + time$ + " [Program run]" +_
      chr$(13) + chr$(10))
   dailydate$ = fnhistdate$

initio:
   if comport = 1_
      then baseaddress = &h3f8_
      else baseaddress = &h2f8

   call printout("Initializing modem..." + chr$(13))
   call initport
   if modeminit$ = "" then modeminit$ = "ATE0H0M0Q0V1X1 S0=1 S2=255 S7=15"

on key(1) gosub f1command
on timer(1) gosub timeupdate

'----------------------------------------------------------------------------

awaitcall:
   call blankscreen : locate 1,1,1
   call setbaud(maxbaud)
   call modemcmd(modeminit$)
   baud$ = "" : chat = 0 : charwaiting = 0 : fullscreen = 1 : msgactivity = 0
   illegaltries = 0 : logon = 0 : nest = 0 : pause = 1 : remote = 0 : ring = 0
   scrnwidth = 0 : seclevel = 0 : timeon = 0 : videowidth = 80 : hidden = 0
   uc = 0 : termstatus = 21 : timeallowed = 255 : carrier = 0 : clockstop = 0
   filenm$ = bbspfx$ + "DAILY.LOG"
   if fnfileexist(filenm$) = 1_
      then open filenm$ for input as #3 :_
           input #3, temp$ :_
           close #3 :_
           if len(temp$) > 8_
              then if left$(temp$,8) <> fnhistdate$_
                      then kill filenm$
   display = 65
   awaitloop:
      locate 1,1,1 : print chr$(display);
      incr display
      if display = 256 then display = 65
      locate 1,1,1 : print chr$(32);
      call carrierchk
      if carrier = 1 then remote = 1 : goto login
      a$ = inkey$
      if a$ = "" then awaitloop
      if ucase$(a$) = "Q" then endprog
      if ucase$(a$) = "T" then termprog
      if ucase$(a$) = "L"_
         then call modemcmd("ATH1") :_
              goto login
      if ucase$(a$) = "S"_
         then call modemcmd("ATH1") :_
              cls : print "DOS shell - type EXIT to return to program." :_
              shell :_
              call hangup :_
              goto awaitcall
      if ucase$(a$) = "C"_
         then awaitcall
      if ucase$(a$) = "I"_
         then call prt("Resetting modem...") :_
              call modemcmd(modeminit$) :_
              call prt("modem initialization string sent." + chr$(13)) :_
              goto awaitloop
      if ucase$(a$) = "U" or a$ = "1"_
         then call modemcmd("ATH1") :_
              call addhistory(fnhistdate$ + " " + time$ + " U") :_
              seclevel = 15 :_
              timer on :_
              goto userdatabase
      if ucase$(a$) = "B" or a$ = "2"_
         then call modemcmd("ATH1") :_
              call addhistory(fnhistdate$ + " " + time$ + " B") :_
              seclevel = 15 :_
              timer on :_
              goto bmmenu
      if a$ = " " or a$ = "?"_
         then call prt("Console commands" + chr$(13)_
                 + chr$(13)) :_
              call prt("[C] Clear screen" + chr$(13)) :_
              call prt("[B] Board maintenance" + chr$(13)) :_
              call prt("[L] Local logon" + chr$(13)) :_
              call prt("[Q] Quit BBS" + chr$(13)) :_
              call prt("[S] DOS shell" + chr$(13)) :_
              call prt("[T] Terminal program" + chr$(13)) :_
              call prt("[U] User database" + chr$(13) + chr$(13)) :_
              goto awaitloop
      goto awaitloop

   termprog:
      termstatus = termstatus and 239
      remote = 0
      call prt(chr$(13) + "*** Black Hole BBS Terminal Program ***" + chr$(13))
      uc = 1
      call inpt("[3]00, [1]200, [2]400, [H]angup, [Q]uit, [T]erm: ",value$,0)
      if value$ = "3"_
         then call setbaud(300) :_
              call prt(chr$(13) + "300 baud." + chr$(13)) :_
              goto termprog
      if value$ = "1"_
         then call setbaud(1200) :_
              call prt(chr$(13) + "1200 baud." + chr$(13)) :_
              goto termprog
      if value$ = "2"_
         then call setbaud(2400) :_
              call prt(chr$(13) + "2400 baud." + chr$(13)) :_
              goto termprog
      if value$ = "H"_
         then call prt(chr$(13) + "Hanging up modem...") :_
              call hangup :_
              call prt("done." + chr$(13)) :_
              goto termprog
      if value$ = "T" then termloop
      if value$ = "Q"_
         then call prt(chr$(13) + "Quitting..." + chr$(13)) :_
              call hangup :_
              termstatus = termstatus or 16 :_
              goto awaitcall
      goto termprog
   termloop:
      status = 128 + (bell * 64) + (fullscreen * 32) + (abortable * 8)_
         + (carrier * 4) + (comport - 1)
      locphrase$ = chr$(status) + chr$(termstatus) + chr$(videowidth - 1)
      remphrase$ = chr$(status + 2) + chr$(termstatus) + chr$(videowidth - 1)
      call prt(chr$(13) + "Terminal mode." + chr$(13) + chr$(13))
      do
         remote = 1
         call readchr(charloc$,charrem$)
         remote = 0
         if charloc$ = chr$(27) then exit loop
         if (charrem$ <> chr$(0)) and (charrem$ <> chr$(10))_
            then call mlprt(locphrase$ + charrem$)
         if charloc$ <> chr$(0)_
            then if (inp(baseaddress + 6) and 128) <> 128_
                    then call mlprt(remphrase$ + charloc$)_
                    else reg 1, 256 + asc(charloc$) :_
                         reg 4, comport - 1 :_
                         call interrupt &h14
      loop
      goto termprog

'--------------------
login:
   key(1) on
   timer on
   fullscreen = 0
   timestart = timeon
   if remote = 0_
      then call prt(chr$(13) + "Connected locally" + chr$(13)) :_
           goto enterloop
   do
      call response(modem$)
   loop until (modem$ = "BAD") or (modem$ = "NO CARRIER")_
      or (instr(modem$,"CONNECT") > 0)
   if modem$ = "BAD"_
      then call addhistory(fnhistdate$ + " " + time$ + " " + "[Modem error]"_
              + chr$(13) + chr$(10)) :_
           call hangup :_
           goto awaitcall
   if modem$ = "NO CARRIER" then call hangup : goto awaitcall
   if modem$ = "CONNECT 2400" then baud$ = "2400"
   if modem$ = "CONNECT 1200" then baud$ = "1200"
   if modem$ = "CONNECT" then baud$ = "300"
   call setbaud(val(baud$))
   delay 1
   call prt(chr$(13) + "Connected at " + baud$ + " baud" + chr$(13))
enterloop:
   call addhistory(fnhistdate$ + " " + time$ + " ")
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\LOGON.MSG",0)
   call prt(chr$(13) + "Welcome to " + bbsname$ + "!" + chr$(13))
enterpw:
   uc = 1
   call inpt(chr$(13) + "Enter your name, handle, or ID # " +_
      "(or NEW)" + chr$(13) + ": ",userid$,20)
   if userid$ = "NEW" then newuser
   hidden = 1 : uc = 1
   call inpt(chr$(13) + "Enter your password" + chr$(13) + ": ",pword$,8)
   if userid$ = "" or pword$ = "" then illegalattempt
   call prt(chr$(13) + "Searching..." + chr$(13))
   if val(userid$) <> 0 and fntruenum(userid$) = 1_
      then userid = val(userid$) : goto checkid
   userid = 0
   counter = 0
   do
      incr counter
      get #2, counter
      call unpad(username$,tempname$)
      call unpad(userhandle$,temphandle$)
      if tempname$ = userid$ or temphandle$ = userid$_
         then userid = counter
   loop until eof(2) or userid > 0
   checkid:
   if userid = 0 then illegalattempt
   get #2, userid
   call unpad(userpassword$,password$)
   if password$ <> pword$ then illegalattempt
   goto welcome

illegalattempt:
   call prt(chr$(13) + "Illegal access attempt!" + chr$(13))
   illegaltries = illegaltries + 1
   if illegaltries = 3_
      then call prt(chr$(13) + "Too many illegal attempts." + chr$(13)) :_
           call addhistory("[Illegal]") :_
           goto logoff_
      else enterpw

'--------------------

$include "newuser.bas"

'--------------------
welcome:
   call unpad(username$,realname$)
   call unpad(userhandle$,handle$)
   call unpad(useraddress$,address$)
   call unpad(usercity$,city$)
   call unpad(userphone$,phone$)
   seclevel = asc(userseclevel$)
   timeallowed = asc(usertimeallowed$)
   timeontoday = asc(usertimeontoday$)
   totalcalls = cvi(usertotalcalls$)
   firstcall$ = userfirstcall$
   if left$(userlastcall$,1) <> chr$(0)_
      then call expanddatetime(userlastcall$,lastcalldate$) :_
           call datetimeformat(lastcalldate$) :_
           if left$(lastcalldate$,8) <> fnhistdate$ then timeontoday = 0
   lastmsgptr = cvi(userlastmsgptr$)
   usertype = asc(usertype$)
   videowidth = asc(uservideo$)
   termstatus = asc(usertermstatus$)
   filter$ = userfilter$
   uploadtotal& = cvl(userupload$)
   downloadtotal& = cvl(userdownload$)

welcparms:
   allnewsub = 0 : filearea = 1
   call setboardlist
   call setarealist
   call statusdraw
   logon = 1 : sysmsgptr = msgptr
   expert = termstatus and 32
   if seclevel < 10 then callernum = callernum + 1
   call addhistory(userhandle$)
   if seclevel < 10_
      then call addhistory(" " + str$(seclevel) + "  ")_
      else call addhistory(str$(seclevel) + "  ")
   if msgactivity = 1 then call addhistory("f+")
   call brdcolor(0)
   call brdcolor(1)
   call brdcolor(44)
   call brdcolor(33)
   call prt(chr$(13) + chr$(13))
   call prt(bbsname$ + " welcomes " + handle$ + ".  You are caller number"_
      + str$(callernum) + ".")
   if totalcalls <> 0_
      then call prt("  You have called here" + str$(totalcalls)_
         + " times before.")
   if fnnormaldate$(lastcalldate$) <> "[No date]"_
      then call prt("  You haven't been on since " +_
         fnnormaldate$(lastcalldate$) + "." + chr$(13))_
      else call prt(chr$(13))
   totalcalls = totalcalls + 1
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\WELCOME.MSG",0)
   call timecheck(overtime)
   if overtime and timeallowed <> 255 then maxtime
   if timeallowed = 255_
      then call prt(chr$(13) + "You have unlimited time for this call." +_
              chr$(13))_
      else call prt(chr$(13) + "Access time for this call"_
             + fntimecnvt$((timeallowed - timeontoday) * 60) + "." + chr$(13))
   if boardlist(0) = 0 and arealist(0) = 0 then mailchk
   newboards = 0 : newareas = 0
   for counter = 1 to boardlist(0)
      if boardptr(boardlist(counter)) >= lastmsgptr_
         then incr newboards
   next counter
   for counter = 1 to arealist(0)
      if areaptr(arealist(counter)) >= lastmsgptr_
         then incr newareas
   next counter
   if newboards = 0 then newareaprt
newboardprt:
   call prt(chr$(13) + "The following subboards have been created since your "_
      + "last logon.  They are now being unfiltered." + chr$(13) + chr$(13))
   for counter = 1 to boardlist(0)
      if boardptr(boardlist(counter)) < lastmsgptr then newsubloop
      if fnfiltered(boardlist(counter)) = 1_
         then mid$(filter$,filterbyte,1) =_
              chr$(asc(mid$(filter$,filterbyte,1)) - 2 ^ filterbit)
      if counter < 10 then call prt(" ")
      call prt(str$(counter) + ".  " + boardname$(boardlist(counter))_
         + chr$(13))
   newsubloop:
   next counter
newareaprt:
   if newareas = 0 then mailchk
   call prt(chr$(13) + "The following file areas have been created since "_
      + "your last logon." + chr$(13) + chr$(13))
   for counter = 1 to arealist(0)
      if areaptr(arealist(counter)) < lastmsgptr then newarealoop
      if counter < 10 then call prt(" ")
      call prt(str$(counter) + ".  " + areaname$(arealist(counter))_
         + chr$(13))
   newarealoop:
   next counter
mailchk:
   email(0) = 0
   get #7, 256
   emailtotal = cvi(emailnum$)
   if emailtotal = 0 then mainmenu
   for counter = 1 to emailtotal
      get #7, counter
      if cvi(emailrecv$) = userid_
         then incr email(0) :_
              email(email(0)) = counter
   next counter
   if email(0) = 0 then mainmenu
   email = 1
   call prt(chr$(13) + "You have" + str$(email(0)) + " letter")
   if email(0) > 1 then call prt("s")
   uc = 1
   call prt(" waiting!" + chr$(13))
   call inpt("Read your mail now? (y/n) ",value$,0)
   if value$ <> "Y" then mainmenu
   call addhistory("r")
   goto emailreceive

'--------------------
mainmenu:
   menucmd$ = "ABCEGHIMTUY"
   if seclevel >= 2 then menucmd$ = menucmd$ + "F"
   if seclevel > = 7 then menucmd$ = menucmd$ + "13"
   if seclevel > = 9 then menucmd$ = menucmd$ + "2"
   menu$ = "1 User database *\2 Board maintenance *\3 File editor *\A All new messages\B Board functions\C Chat with Sysop\E Electronic mail\F File transfers\G Goodbye\H Help\I Information files\M Miscellaneous files\" +_
      "T Terminal configuration\U User listings\Y Your status\"
   call printmenu("Main")
   selection = instr(menucmd$,cmd$)
   call addhistory(cmd$)
   on selection goto mainallnew, boardfunctions, chat, email, goodbye,_
      mainhelp, infofiles, miscfiles, termconfig, userlist, yourstatus,_
      filefunctions, userdatabase, fileeditor, bmmenu

'--------------------
mainallnew:
   gosub allnewmsgs
   goto mainmenu

mainhelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\MAIN.HLP",0)
   goto mainmenu

errorgen:
   call inpt(chr$(13) + "Enter the number of the error to generate: ",_
      value$,3)
   if fntruenum(value$) = 0 or value$ = "" then mainmenu
   error val(value$)
   goto mainmenu

'--------------------

$segment

$include "boardfnc.bas"
$include "filefnc.bas"

$segment

$include "miscfnc.bas"
$include "emailfnc.bas"
$include "sysopfnc.bas"

'--------------------

$segment

maxtime:
   call prt(chr$(13) + chr$(13) + "Daily time limit reached!" + chr$(13))
   call addhistory("[Daily time limit]")
   goto goodbyemsg

timedout:
   call prt(chr$(13) + chr$(13) + "System time-out." + chr$(13))
   call addhistory("[Time-out]")
   goto goodbyemsg

carrierdrop:
   msgactivity = 0 : remote = 0
   call prt("** Lost carrier " + fnhistdate$ + " " + time$ + chr$(13))
   call addhistory("[Lost]")
   close #1
   goto logoff

goodbye:
   uc = 1
   call inpt(chr$(13) + "[F]eedback, [H]angup, [R]eturn to BBS? ",cmd$,0)
   if cmd$ = "F" or cmd$ = "H" or cmd$ = "R"_
      then call addhistory(lcase$(cmd$))
   if cmd$ = "F"_
      then emailid = 1 :_
           emailtitle$ = "Feedback" :_
           gosub emailchkid :_
           goto goodbye
   if cmd$ = "R" then mainmenu
   if cmd$ <> "H" then goodbye
goodbyemsg:
   abortable = 1 : bye = 1
   call fileprt(bbspfx$ + "TEXT\LOGOFF.MSG",0)
   call prt(chr$(13) + "Thanks for calling " + bbsname$ + "!" + chr$(13))
   call prt(chr$(13) + "Time on system" + fntimecnvt$(timeon) + chr$(13))
   call prt(chr$(13) + "Black Hole BBS v. 1.3" + chr$(13))
   call prt("(c) 1987-89 Brad Hufnagel" + chr$(13))
   call prt("Goodbye!" + chr$(13) + chr$(13))
   call brdcolor(0)
   bye = 0

logoff:
   if nest > 0 then call pop : decr nest : goto logoff
   key(1) off
   timer off
   call hangup
   call modemcmd("ATH1")
   call addhistory(" " + time$)
   if baud$ = "2400" then call addhistory(" [2400]")
   if baud$ = "1200" then call addhistory(" [1200]")
   if baud$ = "300" then call addhistory(" [300]")
   if baud$ = "" then call addhistory(" [Local]")
   call addhistory(chr$(13) + chr$(10))
   if logon = 0 then call blankscreen : goto awaitcall

updateuser:
   if msgactivity = 1 then lastmsgptr = sysmsgptr
   lset username$ = realname$
   lset userhandle$ = handle$
   lset useraddress$ = address$
   lset usercity$ = city$
   lset userphone$ = phone$
   lset userpassword$ = password$
   lset userseclevel$ = chr$(seclevel)
   lset usertimeallowed$ = chr$(timeallowed)
   timemins = timeon \ 60
   if timemins = 0 then timemins = 1
   timeontoday = timeontoday + timemins
   if timeontoday > 255 then timeontoday = 255
   lset usertimeontoday$ = chr$(timeontoday)
   lset usertotalcalls$ = mki$(totalcalls)
   call compressdatetime(lastcalldate$)
   lset userlastcall$ = lastcalldate$
   lset userlastmsgptr$ = mki$(lastmsgptr)
   lset usertype$ = chr$(usertype)
   lset uservideo$ = chr$(videowidth)
   lset usertermstatus$ = chr$(termstatus)
   lset userfilter$ = filter$
   lset userupload$ = mkl$(uploadtotal&)
   lset userdownload$ = mkl$(downloadtotal&)
   put #2, userid

   filenm$ = bbspfx$ + "BHPTRS.DAT"
   open filenm$ for output as #4
      print #4, totalusers, callernum, msgptr, closed, timeout, uldlratio,_
         dfltunvaltime, dfltunvallevel, dfltunvaltype, dfltvaltime,_
         msglimit, emaillimit, filelimit
      for counter = 1 to 8
         write #4, typename$(counter), type(counter)
      next counter
      print #4, totalsubboards
      for counter = 1 to 48
         write #4, boardname$(counter), boardowner(counter),_
            boardread(counter), boardpost(counter), boardptr(counter),_
            boardlimit(counter)
      next counter
      print #4, totalareas
      for counter = 1 to 48
         write #4, areaname$(counter), areaowner(counter),_
            areaul(counter), areadl(counter), areaptr(counter)
      next counter
   close #4
   call blankscreen
   goto awaitcall

endprog:
   call addhistory(fnhistdate$ + " " + time$ + " [Quit]" + chr$(13) + chr$(10))
   close #10
   close #9
   close #8
   close #7
   close #6
   close #5
   close #4
   close #3
   close #2
   close #1
   call modemcmd("ATZ")
   call removefossil
   cls
   end

'----------------------------------------------------------------------------

'subroutines

timeupdate:
   if clockstop = 0 then timeon = timeon + 1
   return

f1command:
   remotestatus = remote
   remote = 0
   call prt(chr$(13) + "** Special command: ")
   f1loop:
      f1cmd$ = inkey$
      if f1cmd$ = "" then f1loop	
   f1cmd$ = ucase$(f1cmd$)
   call prt(f1cmd$ + chr$(13))
   f1menucmd$ = "?BCFHLMRSTV"
   f1selection = instr(f1menucmd$,f1cmd$)
   if f1selection = 0_
      then f1bye_
      else on f1selection goto f1help, f1bell, f1chat, f1file,_
         f1hangup, f1level, f1map, f1remote, f1shell, f1time, f1val

f1help:
   call prt("[B]ell    [C]hat    [H]angup  [L]evel   [M]ap" +_
      chr$(13))
   call prt("[R]emote  [S]hell   [T]ime    [V]alidate" + chr$(13))
   goto f1bye

f1bell:
   if bell = 1_
      then bell = 0 : call prt("Bell is now off." + chr$(13))_
      else bell = 1 : call prt("Bell is now on." + chr$(13))
   goto f1bye

f1chat:
   chat = 0
   call prt(chr$(13) + "Now entering chat mode - ESC exits")
   clockstop = 1
   remote = remotestatus
   charcount = 0
   call prt(chr$(13) + "The sysop will see you now." + chr$(13) + chr$(13))
   do
      call readchr(charloc$,charrem$)
      if charloc$ = chr$(27) then clockstop = 0 : exit loop
      if charcount = 79_
         then remotestatus = remote :_
              remote = 0 :_
              call prt(chr$(13)) :_
              remote = remotestatus :_
              charcount = 0
      if charloc$ = chr$(0) then char$ = charrem$ else char$ = charloc$
      if char$ = chr$(127) then char$ = chr$(8)
      select case char$
         case > chr$(31)
            call prt(char$)
            incr charcount
         case = chr$(13)
            call prt(chr$(13))
            charcount = 0
         case = chr$(8)
            if charcount > 0_
               then call prt(chr$(0) + chr$(8) + chr$(32) + chr$(8)) :_
                    decr charcount
      end select
   loop
   call prt(chr$(13) + "Returning to the BBS..." + chr$(13))
   goto f1bye

f1file:
   call prt("Disk filing is now ")
   if dump = 0_
      then dump = 1 : call prt("on." + chr$(13))_
      else dump = 0 : call prt("off." + chr$(13))
   goto f1bye

f1hangup:
   remote = remotestatus
   call hangup
   goto f1bye

f1level:
   call inpt("Level? ",value$,2)
   if value$ = "" then f1bye
   level = val(value$)
   if (level = 0 and instr(value$,"0") = 0) or level < 0 or level > 15_
      then call prt("Invalid security level." + chr$(13)) : goto f1bye
   seclevel = level
   if f1cmd$ = "V" then f1map
   goto f1bye

f1map:
   f1tempmap = 0
   for f1counter = 1 to 8
   if type(f1counter) = 0 then f1mapnext
   f1factor = 2 ^ (f1counter - 1)
   f1mapprompt:
   if (usertype and f1factor) = f1factor_
      then query$ = "Y"_
      else query$ = "N"
   uc = 1
   call inpt(typename$(f1counter) + " (y/n/q) " + query$ + chr$(8),f1value$,0)
   if f1value$ = "Q" then f1bye
   if f1value$ = chr$(13) then f1value$ = query$
   if f1value$ <> "Y" and f1value$ <> "N" then f1mapprompt
   if f1value$ = "Y"_
      then f1tempmap = f1tempmap or f1factor_
      else f1tempmap = f1tempmap and (32767 - f1factor)
   f1mapnext:
   next f1counter
   usertype = f1tempmap
   call setboardlist
   call setarealist
   goto f1bye

f1remote:
   if remotestatus = 0_
      then remotestatus = 1 : call prt("Remote enabled." + chr$(13))_
      else remotestatus = 0 : call prt("Local only." + chr$(13))
   goto f1bye

f1shell:
   clockstop = 1
   remote = remotestatus
   call prt(chr$(13) + "Attention, the Sysop is now entering DOS.  Please " +_
      "stand by..." + chr$(13))
   remote = 0
   call storecursor
   cls
   print "DOS shell - type EXIT to return to program."
   shell
   call restorecursor
   call blankscreen
   cursory = 1
   if logon = 1_
      then call statusdraw
   remote = remotestatus
   call prt(chr$(13) + "Now returning to the BBS." + chr$(13))
   clockstop = 0
   goto f1bye

f1time:
   call inpt("Access time? ",value$,3)
   if fntruenum(value$) = 0 then f1bye
   if value$ = "" then f1bye
   newtime = val(value$)
   if newtime < 0 or newtime > 255_
      then call prt("Invalid time value." + chr$(13)) : goto f1bye
   timeallowed = newtime
   goto f1bye

f1val:
   call prt("Access time =" + str$(dfltvaltime) + " min." + chr$(13))
   timeallowed = dfltvaltime
   goto f1level

f1bye:
   remote = 0
   call prt(chr$(27) + "** Exit" + chr$(13))
   remote = remotestatus
   call flushin
   return

bbserror:
   call prt(chr$(13) + chr$(13) + chr$(7) + "--- System error ---" +_
      chr$(13))
   call prt(chr$(13) + "A system error has occurred.  The system will " +_
      "be shut down until the sysop can fix it." + chr$(13))
   call hangup
   call addhistory("[Error" + str$(err) + " at" + str$(eradr) + "] " +_
      time$ + chr$(13) + chr$(10))
   close #10
   close #9
   close #8
   close #7
   close #6
   close #5
   close #4
   close #3
   close #2
   close #1
   call modemcmd("ATZ")
   call prt(chr$(13) + "System error" + str$(err) + " at location" +_
      str$(eradr) + chr$(13))
   call adderror("System error" + str$(err) + " at location" + str$(eradr))
   if errorboot$ = "Y"_
      then call addhistory(fnhistdate$ + " " + time$ + " [Error reboot]"_
              + chr$(13) + chr$(10)) :_
           call blankscreen :_
           resume initvariables
   call removefossil
   end
