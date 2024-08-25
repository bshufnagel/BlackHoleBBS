'Black Hole BBS miscellaneous functions
'by Brad Hufnagel
'----------------------------------------------------------------------------

chat:
   if seclevel < 5 then chatpage
   uc = 1
   call inpt(chr$(13) + "Is this an emergency chat? (y/n) ",value$,0)
   if value$ <> "Y" then chatpage
   call addhistory("[Emergency]")
   call prt(chr$(13) + "Continuous chat page in progress.  Hit spacebar " +_
      "to cancel." + chr$(13))
   chat = 1 : counter = 0 : abortable = 1 : temp = bell : bell = 1
   do
      incr counter
      call prt(string$(10,7))
   loop until (aborted = 1) or (chat = 0) or (counter = 10)
   abortable = 0 : bell = temp
   if chat = 1_
      then chat = 0 :_
           call prt(chr$(13) + "Sorry, but the sysop must not be around." +_
              chr$(13))
   goto mainmenu

chatpage:
   if chat = 0_
      then chat = 1 :_
           call prt(chr$(13) + "The sysop has been paged to chat." +_
              chr$(13)) :_
           call prt("While paging, you may continue to use the system." +_
              chr$(13))_
      else call prt(chr$(13) + "The sysop has already been paged." + chr$(13))
   goto mainmenu

'--------------------
termconfig:
   gosub termcnf
   goto mainmenu

termcnf:
   nest = nest + 1
   call prt(chr$(13) + "Terminal configuration." + chr$(13))

termlf:
  termstatus = termstatus and 239
  call prt(chr$(13) + "----------" + chr$(13) + "----------" + chr$(13))
  termstatus = termstatus or 16
  call inpt(chr$(13) + "How many lines do you see? (1-2) ",value$,0)
  if value$ = "" then termlf
  if fntruenum(value$) = 0 then termlf
  value = val(value$)
  if value < 1 or value > 2 then termlf
  if value = 1_
     then termstatus = termstatus or 16_
     else termstatus = termstatus and 239

termvideo:
   call inpt(chr$(13) + "What is your video width? (20-80)" +_
      chr$(13) + ": ",termwidth$,2)
   if fntruenum(termwidth$) = 0 then termvideo
   termwidth = val(termwidth$)
   if termwidth < 20 or termwidth > 80_
      then call prt("Invalid width." + chr$(13)) : goto termvideo
   videowidth = termwidth

termnulls:
   call inpt(chr$(13) + "How many nulls do you need? (0 - 15)" +_
      chr$(13) + ": ",termnulls$,2)
   if fntruenum(termnull$) = 0 then termnulls
   termnulls = val(termnulls$)
   if termnulls < 0 or termnulls > 15 or termnulls$ = ""_
      then call prt("Invalid value." + chr$(13)) : goto termnulls
   termstatus = (termstatus and 240) or termnulls

termbye:
   nest = nest - 1
   return

'--------------------
infofiles:
   menucmd$ = "ABIQRW"
   menu$ = "A Advertisement\B BBS's around town\I Information about system\Q Quit\R Rules and regulations\W Welcome message\"
   abortable = 1
   call printmenu("Information files")
   selection = instr(menucmd$,cmd$)
   abortable = 1
   call addhistory(lcase$(cmd$))
   on selection goto adprt, bbsprt, infoprt, mainmenu, rulesprt, welcomeprt

adprt:
   call fileprt(bbspfx$ + "TEXT\AD.MSG",0)
   goto infofiles

bbsprt:
   call fileprt(bbspfx$ + "TEXT\BBS.MSG",0)
   goto infofiles

infoprt:
   call fileprt(bbspfx$ + "TEXT\INFO.MSG",0)
   goto infofiles

rulesprt:
   call fileprt(bbspfx$ + "TEXT\RULES.MSG",0)
   goto infofiles

welcomeprt:
   call fileprt(bbspfx$ + "TEXT\WELCOME.MSG",0)
   goto infofiles

'--------------------
miscfiles:
   call inpt(chr$(13) + "Select file # to read, or ? [CR quits]: ",value$,3)
   if value$ = "" then mainmenu
   if fntruenum(value$) = 0 and value$ <> "?" then miscfiles
   value = val(value$)
   suffix$ = right$(str$(value),len(str$(value)) - 1)
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\MISC\MISC." + suffix$,0)
   goto miscfiles

'--------------------
userlist:
   menucmd$ = "ADHQ"
   menu$ = "A All users\D Date listing\H Help\Q Quit\"
   call printmenu("User listings")
   selection = instr(menucmd$,cmd$)
   call addhistory(lcase$(cmd$)) :_
   on selection goto userall, userdate, userhelp, mainmenu

userall:
   call prt(chr$(13))
   abortable = 1 : listid = 0
   do
      incr listid
      get #2, listid
      if left$(username$,1) = chr$(1) or (usertype and asc(usertype$)) = 0_
         then userallloop
      call unpad(userhandle$,temphandle$)
      call prt("#")
      if aborted then exit loop
      if (listid < 10) then call prt(" ")
      if aborted then exit loop
      if (listid < 100) then call prt(" ")
      if aborted then exit loop
      call prt(str$(listid) + "  " + temphandle$ + chr$(13))
   userallloop:
   loop until eof(2) or aborted = 1
   goto userlist

userdate:
   call prt(chr$(13))
   abortable = 1 : listid = 0
   do
      incr listid
      get #2, listid
      if left$(username$,1) = chr$(1) or (usertype and asc(usertype$)) = 0_
         then userdateloop
      call unpad(userhandle$,temphandle$)
      call expanddatetime(userlastcall$,listlastcall$) :_
      listlastcall$ = left$(listlastcall$,6) :_
      call datetimeformat(listlastcall$) :_
      call prt(listlastcall$ + "  " + temphandle$ + chr$(13))
   userdateloop:
   loop until eof(2) or aborted = 1
   goto userlist

userhelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\USERLIST.HLP",0)
   goto userlist

'--------------------
yourstatus:
   menucmd$ = "CGHPQSXY"
   menu$ = "C Change handle\G ANSI graphics on/off\H Help\P Password change\Q Quit\S System status\X Expert user mode\Y Your status\"
   call printmenu("Your status")
   selection = instr(menucmd$,cmd$)
   call addhistory(lcase$(cmd$)) :_
   on selection goto handlechg, graphicschg, yourstatushelp, passwordchg,_
      mainmenu, systemstatus, expertmode, yourstats

graphicschg:
   call prt(chr$(13) + "ANSI graphics are now ")
   if (termstatus and 64) = 0_
      then termstatus = termstatus or 64 :_
           call prt("enabled." + chr$(13)) :_
           call brdcolor(0) : call brdcolor(1) : call brdcolor(44)_
      else call brdcolor(0) :_
           termstatus = termstatus and 191 :_
           call prt("disabled." + chr$(13))
   goto yourstatus

yourstatushelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\YOURSTAT.HLP",0)
   goto yourstatus

handlechg:
   call prt(chr$(13) + "Your current handle is " + handle$ + "." + chr$(13))
   uc = 1 : call inpt("Enter your new handle." + chr$(13) + ": ",value$,20)
   if value$ = "" then yourstatus
   editid = 0 : badhandle = 0
   do
      incr editid
      get #2, editid
      if left$(username$,1) = chr$(1) then handleloop
      call unpad(userhandle$,temphandle$)
      if value$ = temphandle$ then badhandle = 1
   handleloop:
   loop until eof(2) or badhandle = 1
   if badhandle_
      then call prt(chr$(13) + "That handle is already in use.  Try again." +_
         chr$(13)) :_
           goto handlechg
   call addhistory("[Handle " + handle$ + " to " + value$ + "]")
   handle$ = value$
   call prt(chr$(13) + "You are now " + handle$ + "." + chr$(13))
   goto yourstatus

passwordchg:
   uc = 1 : call inpt(chr$(13) + "Enter your current password." + chr$(13) +_
      ": ",value$,8)
   if value$ = ""_
      then call prt("Password not changed." + chr$(13)) :_
           goto yourstatus
   if password$ <> value$_
      then call prt("Invalid password." + chr$(13)) :_
           goto yourstatus
   uc = 1 : call inpt(chr$(13) + "Enter your new password." + chr$(13) +_
      ": ",value$,8)
   if value$ = ""_
      then call prt("Password not changed." + chr$(13)) :_
      goto yourstatus
   password$ = value$
   call addhistory("[Password changed]")
   call prt(chr$(13) + "Your password is now " + password$ + ".  Don't " +_
      "forget it!" + chr$(13))
   goto yourstatus

systemstatus:
   currentdate$ = left$(date$,6) + right$(date$,2) + " " + left$(time$,5)
   call prt(chr$(13) + "Current date/time: " +_
      fnnormaldate$(currentdate$) + chr$(13))
   call prt("Total caller count:" + str$(callernum) + chr$(13))
   call prt("Number of active users:" + str$(totalusers) + chr$(13))
   call prt("Number of public messages:")
   totalmsgs = 0
   for counter = 1 to 48
      get #6, 256 * counter
      totalmsgs = totalmsgs + cvi(msgnum$)
   next counter
   call prt(str$(totalmsgs) + chr$(13))
   call prt("Number of private letters:")
   get #7, 256
   call prt(str$(cvi(emailnum$)) + chr$(13))
   if seclevel >= 2_
      then call prt("Number of files:") :_
           totalfiles = 0 :_
           for counter = 1 to 48 :_
              get #8, 512 * counter :_
              totalfiles = totalfiles + cvi(filenum$) :_
           next counter :_
           call prt(str$(totalfiles) + chr$(13))
   goto yourstatus

expertmode:
   if expert_
      then termstatus = termstatus and 223 : expert = 0 :_
           call prt(chr$(13) + "Expert user mode is now off." + chr$(13))_
      else termstatus = termstatus or 32 : expert = 1 :_
           call prt(chr$(13) + "Expert user mode is now on." + chr$(13))
   goto yourstatus

yourstats:
   call prt(chr$(13) + "User #" + str$(userid) + chr$(13))
   call prt("Name: " + realname$ + chr$(13))
   if handle$ <> realname$_
      then call prt("Handle: " + handle$ + chr$(13))
   call prt("Security level:" + str$(seclevel) + chr$(13))
   call prt("Daily time limit:" + str$(timeallowed) + " min" + chr$(13))
   call prt("Time on today:" + fntimecnvt$((timeontoday * 60) + timeon) +_
      chr$(13))
   call prt("Calls to system:" + str$(totalcalls) + chr$(13))
   if left$(lastcalldate$,1) <> chr$(0)_
      then call prt("Last call on: " + fnnormaldate$(lastcalldate$) +_
           chr$(13))
   call prt("Video width:" + str$(videowidth) + chr$(13))
   call prt("ANSI graphics are ")
   if (termstatus and 64) = 64_
      then call prt("enabled" + chr$(13))_
      else call prt("disabled" + chr$(13))
   call prt("Files uploaded:" + str$(uploadtotal&) + "K" + chr$(13))
   call prt("Files downloaded:" + str$(downloadtotal&) + "K" + chr$(13))
   call prt("UL/DL ratio:" + str$(fnuldlratio(uploadtotal&,downloadtotal&)) +_
      "%" + chr$(13))
   call prt("User types:  ")
   start = 0
   for counter = 1 to 8
   if (type(counter) = 1) and ((usertype and (2 ^ (counter - 1)))_
      = (2 ^ (counter - 1)))_
      then if start = 0_
              then start = 1 : call prt(typename$(counter))_
              else call prt(", " + typename$(counter))
   next counter
   call prt(chr$(13))
   goto yourstatus
