'Black Hole BBS sysop functions
'by Brad Hufnagel
'----------------------------------------------------------------------------

userdatabase:
   menucmd$ = "12345678EHRQ"
   if logon = 0 then menucmd$ = menucmd$ + "G"
   menu$ = "1 Security level\2 Video width\3 Calls to system\4 Date of last call\5 Date of first call\6 Time on today\7 Access time\8 Phone number (last 4 digits)\E Edit users\G Global user typing\H Handle search\R Real name match\Q Quit\"
   call printmenu("User database")
   search$ = cmd$
   selection = instr(menucmd$,cmd$)
   call addhistory(lcase$(cmd$))
   searchlist$ = "E"
   on selection goto userseclevel, uservideowidth, usercalls, userlast,_
      userfirst, usermins, usertime, userphone, useredit, userhandle,_
      username, userquit, userglobal

userquit:
   if logon = 1_
      then mainmenu_
      else call prt(chr$(13) + "Hanging up modem..." + chr$(13)) :_
           timer off :_
           goto logoff

userseclevel:
   call inpt(chr$(13) + "Enter security level: ",value$,2)
   if value$ = "" then userdatabase
   searchvalue = val(value$)
   if searchvalue < 0 or searchvalue > 15_
      then call prt("Invalid security level." + chr$(13)) : goto userdatabase
   goto usersearch

uservideowidth:
   call inpt(chr$(13) + "Enter video width: ",value$,2)
  if value$ = "" then userdatabase
   searchvalue = val(value$)
   if searchvalue < 20 or searchvalue > 80_
      then call prt("Invalid video width." + chr$(13)) : goto userdatabase
   goto usersearch

usercalls:
   call inpt(chr$(13) + "Enter number of calls: ",value$,4)
   if fntruenum(value$) = 0 then userdatabase
   if value$ = "" then userdatabase
   searchvalue = val(value$)
   goto usersearch

userlast:
   call inpt(chr$(13) + "Enter last call date: ",value$,6)
   if value$ = "" then userdatabase
   baddate = 0
   for counter = 1 to 6
      chkchar$ = mid$(value$,counter,1)
      if asc(chkchar$) < 48 or asc(chkchar$) > 57 then baddate = 1
   next counter
   if len(value$) < 6 then baddate = 1
   if val(right$(value$,2)) < 80 then baddate = 1
   if baddate then call prt("Invalid date." + chr$(13)) : goto userdatabase
   searchvalue = ((val(right$(value$,2)) - 80) * 1000) +_
      (val(left$(value$,2)) * 50) + val(mid$(value$,3,2))
   goto usersearch

userfirst:
   call inpt(chr$(13) + "Enter first call date: ",value$,6)
   if value$ = "" then userdatabase
   baddate = 0
   for counter = 1 to 6
      chkchar$ = mid$(value$,counter,1)
      if asc(chkchar$) < 48 or asc(chkchar$) > 57 then baddate = 1
   next counter
   if len(value$) < 6 then baddate = 1
   if val(right$(value$,2)) < 80 then baddate = 1
   if baddate then call prt("Invalid date." + chr$(13)) : goto userdatabase
   searchvalue = ((val(right$(value$,2)) - 80) * 1000) +_
      (val(left$(value$,2)) * 50) + val(mid$(value$,3,2))
   goto usersearch

usermins:
   call inpt(chr$(13) + "Enter length in minutes: ",value$,3)
   if fntruenum(value$) = 0 then userdatabase
   if value$ = "" then userdatabase
   searchvalue = val(value$)
   goto usersearch

usertime:
   call inpt(chr$(13) + "Enter access time to search for: ",value$,3)
   if fntruenum(value$) = 0 then userdatabase
   if value$ = "" then userdatabase
   searchvalue = val(value$)
   goto usersearch

userphone:
   call inpt(chr$(13) + "Enter last 4 digits of phone number: ",value$,4)
   if val(value$) = 0 and phone$ <> "0000" then userdatabase
   if value$ = "" then userdatabase
   searchvalue = val(value$)
   goto usersearch

usersearch:
   uc = 1
   call inpt("Search for <, >, or = ? ",searchtype$,0)
   if searchtype$ <> "<" and searchtype$ <> ">" and searchtype$ <> "="_
      then userdatabase
   uc = 1
   call inpt(chr$(13) + "[E]dit or [L]ist? ",searchlist$,0)
   if searchlist$ <> "E" and searchlist$ <> "L" then userdatabase
   if searchlist$ = "L" then call prt(chr$(13))
   editid = 0
   do
      nulluser = 0
      incr editid
      chkid& = editid
      if (chkid& * 134) > lof(2) then exit loop
      get #2, editid
      if left$(username$,1) = chr$(1) then usersearchloop
      select case search$
         case = "1"
            editvalue = asc(userseclevel$)
         case = "2"
            editvalue = asc(uservideo$)
         case = "3"
            editvalue = cvi(usertotalcalls$)
         case = "4"
            call expanddatetime(userlastcall$,edit$)
            call datetimeformat(edit$)
            editvalue = ((val(mid$(edit$,7,2)) - 80) * 1000) +_
               (val(left$(edit$,2)) * 50) + val(mid$(edit$,4,2))
         case = "5"
            call expanddatetime(userfirstcall$,edit$)
            call datetimeformat(edit$)
            editvalue = ((val(right$(edit$,2)) - 80) * 1000) +_
               (val(left$(edit$,2)) * 50) + val(mid$(edit$,4,2))
         case = "6"
            editvalue = asc(usertimeontoday$)
         case = "7"
            editvalue = asc(usertimeallowed$)
         case = "8"
            editvalue = val(right$(userphone$,4))
      end select

      select case searchtype$
         case = ">"
            if editvalue > searchvalue then gosub edituser
         case = "<"
            if editvalue < searchvalue then gosub edituser
         case = "="
            if editvalue = searchvalue then gosub edituser
      end select

      if aborted then exit loop

   usersearchloop:
   loop until eof(2) or nulluser = 1
   goto userdatabase

userglobal:
   if systype = 0_
      then call prt(chr$(13) + "No user types have been activated." +_
         chr$(13)) : goto userdatabase
   call prt(chr$(13) + "For each type, indicate I (include), " +_
      "E (exclude), D (don't care), Q (quit):" + chr$(13) + chr$(13))
   includetype = 0 : excludetype = 0
   for counter = 1 to 8
      factor = 2 ^ (counter - 1)
      if type(counter) = 0 then globalsetup
      globalinput:
      uc = 1
      call inpt(typename$(counter) + " (I,E,D,Q) ",value$,0)
      if value$ = "I"_
         then includetype = includetype or factor : goto globalsetup
      if value$ = "E"_
         then excludetype = excludetype or factor : goto globalsetup
      if value$ = "D" then globalsetup
      if value$ = "Q" then userdatabase
      goto globalinput
   globalsetup:
   next counter
   excludetype = 255 - excludetype
   call prt(chr$(13) + "Editing user #")
   editid = 0
   do
      incr editid
      chkid& = editid
      if (chkid& * 134) > lof(2) then exit loop
      get #2, editid
      if left$(username$,1) = chr$(1) then globaloutput
      call prt(str$(editid))
      edittype = asc(usertype$)
      edittype = edittype or includetype
      edittype = edittype and excludetype
      lset usertype$ = chr$(edittype)
      put #2, editid
      call prt(string$(len(str$(editid)),8))
   globaloutput:
   loop until eof(2)
   call prt(str$(editid) + "...done!" + chr$(13))
   goto userdatabase

userhandle:
   uc = 1
   call inpt(chr$(13) + "Enter text to search for." + chr$(13) +_
      ": ",chkhandle$,20)
   if chkhandle$ = "" then userdatabase
   nulluser = 0 : editid = 0
   do
      incr editid
      get #2, editid
      if left$(username$,1) = chr$(1) then userhandleloop
      call unpad(userhandle$,temphandle$)
      if instr(temphandle$,chkhandle$) > 0 then gosub edituser
      if nulluser = 1 then exit loop
   userhandleloop:
   loop until eof(2)
   goto userdatabase

username:
   uc = 1
   call inpt(chr$(13) + "Enter text to search for." + chr$(13) +_
      ": ",chkname$,20)
   if chkname$ = "" then userdatabase
   nulluser = 0 : editid = 0
   do
      incr editid
      get #2, editid
      if left$(username$,1) = chr$(1) then usernameloop
      if instr(username$,chkname$) > 0 then gosub edituser
      if nulluser = 1 then exit loop
   usernameloop:
   loop until eof(2)
   goto userdatabase

useredit:
   editid = 0
   uc = 1
   call inpt(chr$(13) + "Enter name, handle, or user ID # [CR quits]: "_
      ,edit$,20)
   if edit$ = "" then userdatabase
   if (val(edit$) <> 0) and (len(edit$) < 5)_
      then editid = val(edit$) : goto editgetdata
   counter = 0
   do
      incr counter
      get #2, counter
      call unpad(username$,tempname$)
      call unpad(userhandle$,temphandle$)
      if edit$ = tempname$ or edit$ = temphandle$_
         then editid = counter : exit loop
   loop until eof(2)
   if editid = 0_
      then call prt(chr$(13) + edit$ + " not found." + chr$(13)) :_
           goto userdatabase

editgetdata:
   chkid& = editid
   if (chkid& * 134) > lof(2) then userdatabase
   nulluser = 0
   gosub edituser
   if nulluser = 0 then_
      editid = editid + 1 :_
      goto editgetdata
   goto useredit

edituser:
   nest = nest + 1
   get #2, editid
   if left$(username$,1) = chr$(1) then nulluser = 0 : return
   call unpad(username$,editname$)
   call unpad(userhandle$,edithandle$)
   call unpad(useraddress$,editaddress$)
   call unpad(usercity$,editcity$)
   call unpad(userphone$,editphone$)
   call unpad(userpassword$,editpassword$)
   editseclevel = asc(userseclevel$)
   edittimeallowed = asc(usertimeallowed$)
   edittimeontoday = asc(usertimeontoday$)
   edittotalcalls = cvi(usertotalcalls$)
   call expanddatetime(userfirstcall$,editfirstcall$)
   call datetimeformat(editfirstcall$)
   call expanddatetime(userlastcall$,editlastcall$)
   call datetimeformat(editlastcall$)
   editlastmsgptr = cvi(userlastmsgptr$)
   edittype = asc(usertype$)
   if seclevel < 9 then edittype = edittype and usertype
   editvideowidth = asc(uservideo$)
   edittermstatus = asc(usertermstatus$)
   edituploadtotal& = cvl(userupload$)
   editdownloadtotal& = cvl(userdownload$)
   if (editseclevel >= seclevel) and (logon = 1)_
      then call prt("Access to " + edithandle$ + " not permitted." +_
              chr$(13)) : return

editlist:
   if searchlist$ <> "L" then editedit
   abortable = 1
   call prt("#" + str$(editid))
   if aborted then editbye
   for spcprt = 1 to 4 - len(str$(editid))
      call prt(" ")
      if aborted then exit for
   next spcprt
   if aborted then editbye
   call prt("  " + userhandle$)
   if aborted then editbye
   if editseclevel < 10 then call prt(" ")
   if aborted then editbye
   call prt(str$(editseclevel) + "  ")
   if aborted then editbye
   call prt("(" + left$(editphone$,3) + ") " + mid$(editphone$,4,3)_
      + "-" + right$(editphone$,4) + chr$(13))
   goto editbye

editedit:
   call prt(chr$(13) + "User #           " + str$(editid) + chr$(13))
   call prt("Handle            " + edithandle$ + chr$(13))
   call prt("Real name         " + editname$ + chr$(13))
   call prt("Address           " + editaddress$ + chr$(13))
   call prt("City, state       " + editcity$ + chr$(13))
   call prt("Phone number      (" + left$(editphone$,3) + ") " +_
      mid$(editphone$,4,3) + "-" + right$(editphone$,4) + chr$(13))
   if seclevel > 8 then call prt("Password          " + editpassword$ +_
      chr$(13))
   call prt("Security level   " + str$(editseclevel) + chr$(13))
   call prt("Daily time limit " + str$(edittimeallowed) + " min" + chr$(13))
   call prt("Time on today    " + str$(edittimeontoday) + " min" + chr$(13))
   call prt("Calls to system  " + str$(edittotalcalls) + chr$(13))
   call prt("Member since      " + fnnormaldate$(editfirstcall$) + chr$(13))
   call prt("Last call at      " + fnnormaldate$(editlastcall$) + chr$(13))
   call prt("Last message ptr." + str$(editlastmsgptr) + chr$(13))
   call prt("Files uploaded   " + str$(edituploadtotal&) + "K" + chr$(13))
   call prt("Files downloaded " + str$(editdownloadtotal&) + "K" + chr$(13))
   call prt("UL/DL ratio      " +_
      str$(fnuldlratio(edituploadtotal&,editdownloadtotal&)) + "%" + chr$(13))
   call prt(chr$(13) + "User types:  ")
   start = 0
   for counter = 1 to 8
      if (type(counter) = 1) and ((edittype and 2 ^ (counter - 1))_
         = 2 ^ (counter - 1))_
         then if start = 0_
                 then start = 1 : call prt(typename$(counter))_
                 else call prt(", " + typename$(counter))
   next counter
   call prt(chr$(13) + chr$(13) + "Terminal settings: " +_
      str$(editvideowidth) + " columns, ")
   if (edittermstatus and 64) = 64_
      then call prt("ANSI, ")
   if (edittermstatus and 32) = 32_
      then call prt("X-pert, ")
   if (edittermstatus and 16) = 16_
      then call prt("L/F's,")_
      else call prt("no L/F's,")
   call prt(str$(edittermstatus and 15) + " nulls" + chr$(13))

editcmd:
   editcmd$ = "ACDHLMNPTQRV"
   uc = 1
   call prt(chr$(13) + "[A]ddress [C]ity    [D]elete  [H]andle  [L]evel   " +_
      "[M]ap" + chr$(13))
   call inpt("[N]ame    [P]hone   [T]ime    [Q]uit    [R]elist  [V]alidate? ",_
      cmd$,0)
   if cmd$ = chr$(13) then editbye
   editselection = instr(editcmd$,cmd$)
   if editselection = 0_
      then editcmd_
      else on editselection goto editaddr, editcity, editdel, edithandle,_
         editlevel, editmap, editname, editphone, edittime, editquit,_
         editlist, editval

editaddr:
   uc = 1
   call inpt(chr$(13) + "Enter new address." + chr$(13) + ": ",tempaddress$,22)
   if tempaddress$ = "" then editcmd
   editaddress$ = tempaddress$
   lset useraddress$ = tempaddress$
   put #2, editid
   if userid = editid then address$ = tempaddress$
   goto editcmd

editcity:
   uc = 1
   call inpt(chr$(13) + "Enter new city." + chr$(13) + ": ",tempcity$,22)
   if tempcity$ = "" then editcmd
   editcity$ = tempcity$
   lset usercity$ = tempcity$
   put #2, editid
   if userid = editid then city$ = tempcity$
   goto editcmd

editdel:
   if (editid = userid) and (logon = 1)_
      then call prt(chr$(13) + "You can't delete yourself!" + chr$(13)) :_
           goto editcmd
   uc = 1
   call inpt(chr$(13) + "Are you sure? (y/n) ",value$,0)
   if value$ <> "Y" then editcmd
   lset username$ = chr$(1)
   lset userhandle$ = chr$(1)
   put #2, editid
   for counter = 1 to 255
      get #7, counter
      if cvi(emailrecv$) = editid_
         then msg$ = str$(cvi(emailnum$)) :_
              filenm$ = bbspfx$ + "EMAIL\" + right$(msg$,len(msg$) - 1)_
                 + ".EML" :_
              if fnfileexist(filenm$) = 1 then kill filenm$ :_
              for counter2 = counter + 1 to 255 :_
                 get #7, counter2 :_
                 put #7, counter2 - 1 :_
              next counter2 :_
              get #7, 256 :_
              value = cvi(emailnum$) :_
              decr value :_
              lset emailnum$ = mki$(value) :_
              put #7, 256
   next counter
   for counter = 1 to 48
      if boardowner(counter) = editid then boardowner(counter) = 1
      if areaowner(counter) = editid then areaowner(counter) = 1
   next counter
   decr totalusers
   call prt("Deleted!" + chr$(13))
   goto editbye

edithandle:
   uc = 1
   call inpt(chr$(13) + "Enter new handle." + chr$(13) + ": ",temphandle$,20)
   if temphandle$ = "" then editcmd
   edithandle$ = temphandle$
   lset userhandle$ = edithandle$
   put #2, editid
   if userid = editid then handle$ = temphandle$
   goto editcmd

editlevel:
   call inpt(chr$(13) + "Enter security level." + chr$(13) + ": ",templevel$,2)
   if templevel$ = "" then editcmd
   templevel = val(templevel$)
   if (templevel = 0 and instr(templevel$,"0") = 0)_
      or templevel < 0 or templevel > 15_
      or (templevel >= seclevel and userid <> 1 and logon = 1)_
      then call prt("Invalid level." + chr$(13)) : goto editcmd
   editseclevel = templevel
   lset userseclevel$ = chr$(templevel)
   if cmd$ = "V" then editmap
   put #2, editid
   if userid = editid then seclevel = templevel
   goto editcmd

editmap:
   call prt(chr$(13))
   tempmap = 0
   for counter = 1 to 8
      factor = 2 ^ (counter - 1)
      if type(counter) = 0 or (((usertype and factor) <> factor)_
         and seclevel < 9)_
         then editmapnext
      editmapprompt:
      if (edittype and factor) = factor_
         then query$ = "Y"_
         else query$ = "N"
      uc = 1
      call inpt(typename$(counter) + " (y/n/q) " + query$ + chr$(8),value$,0)
      if value$ = "Q" then exit for
      if value$ = chr$(13) then value$ = query$
      if value$ <> "Y" and value$ <> "N" then editmapprompt
      if value$ = "Y"_
         then tempmap = tempmap or factor_
         else tempmap = tempmap and (32767 - factor)
   editmapnext:
   next counter
   if value$ = "Q" then editcmd
   edittype = tempmap
   lset usertype$ = chr$(edittype)
   put #2, editid
   if userid = editid_
      then usertype = edittype :_
           call setboardlist :_
           call setarealist
   goto editcmd

editname:
   uc = 1
   call inpt(chr$(13) + "Enter new name." + chr$(13) + ": ",tempname$,20)
   if tempname$ = "" then editcmd
   editname$ = tempname$
   lset username$ = editname$
   put #2, editid
   if userid = editid then realname$ = tempname$
   goto editcmd

editphone:
   call inpt(chr$(13) + "Enter new phone no." + chr$(13) + ": ",tempphone$,10)
   if tempphone$ = "" or len(tempphone$) < 10 then editcmd
   badphone = 0
   for counter = 1 to 10
      charval = asc(mid$(tempphone$,counter,1))
      if charval < 48 or charval > 57 then badphone = 1
   next counter
   if badphone then call prt("Invalid phone number." + chr$(13)) : goto editcmd
   editphone$ = tempphone$
   lset userphone$ = tempphone$
   put #2, editid
   if userid = editid then phone$ = tempphone$
   goto editcmd

edittime:
   call inpt(chr$(13) + "Enter access time." + chr$(13) + ": ",temptime$,3)
   if fntruenum(temptime$) = 0 then editcmd
   if temptime$ = "" then editcmd
   temptime = val(temptime$)
   if (temptime = 0 and instr(temptime$,"0") = 0)_
      or temptime < 0 or temptime > 255_
      then call prt("Invalid access time." + chr$(13)) : goto editcmd
   edittimeallowed = temptime
   lset usertimeallowed$ = chr$(temptime)
   put #2, editid
   if userid = editid and logon = 1 then timeallowed = temptime
   goto editcmd

editval:
   call prt(chr$(13) + "Access time =" + str$(dfltvaltime) + " min.")
   edittimeallowed = dfltvaltime
   lset usertimeallowed$ = chr$(dfltvaltime)
   goto editlevel

editquit:
   nulluser = 1

editbye:
   nest = nest - 1
   return

'--------------------
bmmenu:
   menucmd$ = "ACDELNQRSTUVX*"
   menu$ = "A Activate/deactivate user types\C Configuration of system\D Download ratio set\E Email base size\L List user types\N New callers toggle\Q Quit\R Rename user type\S System history\" +_
      "T Time-out value\U Unvalidated user defaults\V Validated user defaults\X Error log\* Update total users\"
   call printmenu("Board maintenance")
   selection = instr(menucmd$,cmd$)
   on selection goto bmactive, bmconfig, bmratio, bmemail, bmlist,_
      bmnewcallers, bmquit, bmrename, bmhistory, bmtimeout, bmunvaldflt,_
      bmvaltime, bmerrorlog, bmrecon

bmquit:
   if logon = 1_
      then mainmenu
   call prt(chr$(13) + "Hanging up modem and updating system data..."_
      + chr$(13))
   timer off
   filenm$ = bbspfx$ + "BHPTRS.DAT"
   open filenm$ for output as #4
      print #4, totalusers, callernum, msgptr, closed, timeout, uldlratio,_
         dfltunvaltime, dfltunvallevel, dfltunvaltype, dfltvaltime,_
         msglimit,  emaillimit, filelimit
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
   goto logoff

bmactive:
   call prt(chr$(13) + "Activate/deactivate user types." + chr$(13))
   call inpt(chr$(13) + "Which type [1 - 8] ",value$,1)
   if value$ = "" then bmmenu
   value = val(value$)
   if value < 1 or value > 8_
      then call prt("Invalid user type." + chr$(13)) : goto bmmenu
   call prt(chr$(13) + typename$(value) + " is now ")
   if type(value) = 1_
      then type(value) = 0 : call prt("deactivated." + chr$(13))_
      else type(value) = 1 : call prt("activated." + chr$(13))
   call setboardlist
   systype = 0
   for counter = 1 to 8
      if type(counter) = 1 then systype = systype + (2 ^ (counter - 1))
   next counter
   goto bmmenu

bmconfig:
   call prt(chr$(13) + "Board name is " + bbsname$ + "." + chr$(13))
   call prt("Email base size" + str$(emaillimit) + " letters." + chr$(13))
   call prt("System is ")
   if closed_
      then call prt("closed ")_
      else call prt("open ")
   call prt("to new callers." + chr$(13))
   call prt("System message pointers are" + str$(sysmsgptr) + "," +_
      str$(msgptr) + chr$(13))
   call prt("Upload/download ratio" + str$(uldlratio) + "%" + chr$(13))
   call prt("Time-out value" + str$(timeout) + " sec." + chr$(13))
   call prt("Validated user default time" + str$(dfltvaltime) + " min" +_
      chr$(13))
   call prt("Unvalidated user defaults:" + chr$(13))
   call prt("   Auto-feedback is ")
   if (dfltunvallevel and 128) = 128_
      then call prt("on" + chr$(13))_
      else call prt("off" + chr$(13))
   call prt("   Time limit" + str$(dfltunvaltime) + " min" + chr$(13))
   call prt("   Security level" + str$(dfltunvallevel and 15) + chr$(13))
   call prt("   User types: ")
   start = 0
   for counter = 1 to 8
   if (type(counter) = 1) and (dfltunvaltype and (2 ^ (counter - 1)))_
      = (2 ^ (counter - 1))_
      then if start = 0_
              then start = 1 : call prt(typename$(counter))_
              else call prt(", " + typename$(counter))
   next counter
   call prt(chr$(13))
   goto bmmenu

bmemail:
   call prt(chr$(13) + "Current email base size is" + str$(emaillimit) +_
      " letters." + chr$(13))
   uc = 1
   call inpt(chr$(13) + "Do you want to change this? (y/n) ",value$,0)
   if value$ <> "Y" then bmmenu
   call inpt(chr$(13) + "Enter new email base size: ",value$,3)
   if fntruenum(value$) = 0 then bmmenu
   if value$ = "" then bmmenu
   value = val(value$)
   if value < 1 or value > 255_
      then call prt(chr$(13) + "Invalid size." + chr$(13)) : goto bmmenu
   emaillimit = value
   get #7, 256
   emailtotal = cvi(emailnum$)
   if emaillimit >= emailtotal_
      then emailbaseprt
   call prt(chr$(13) + "Deleting old email...")
   emailend = emailtotal - emaillimit
   for counter = 1 to emailend
      get #7, counter
      msg$ = str$(cvi(emailnum$))
      filenm$ = bbspfx$ + "EMAIL\" + right$(msg$,len(msg$) - 1) + ".EML"
      if fnfileexist(filenm$) = 1 then kill filenm$
      decr emailtotal
   next counter
   for counter = emailend + 1 to 255
      get #7, counter
      put #7, counter - 1
   next counter
   lset emailnum$ = mki$(emailtotal)
   put #7, 256
   call prt("done!" + chr$(13))
   emailbaseprt:
   call prt(chr$(13) + "Email base size is now" + str$(emaillimit) +_
      " letters." + chr$(13))
   goto bmmenu

bmerrorlog:
   call prt(chr$(13))
   abortable = 1
   filenm$ = bbspfx$ + "ERROR.LOG"
   if fnfileexist(filenm$) = 0_
      then call prt("ERROR.LOG not found." + chr$(13)) :_
           goto bmmenu
   open filenm$ for input as #4
   do while not eof(4) and aborted = 0
      line input #4, text$
      call prt(text$ + chr$(13))
   loop
   close #4
   goto bmmenu

bmhistory:
   uc = 1
   call inpt(chr$(13) + "[C]omprehensive or [D]aily history? ",value$,0)
   if value$ <> "C" and value$ <> "D" then bmmenu
   if value$ = "C"_
      then temp$ = "HISTORY.LOG"_
      else temp$ = "DAILY.LOG"
   filenm$ = bbspfx$ + temp$
   call prt(chr$(13))
   abortable = 1
   if fnfileexist(filenm$) = 0_
      then call prt(temp$ + " not found." + chr$(13)) :_
           goto bmmenu
   open filenm$ for input as #4
   do while not eof(4) and aborted = 0
      line input #4, text$
      call prt(text$ + chr$(13))
   loop
   close #4
   goto bmmenu

bmlist:
   call prt(chr$(13))
   for counter = 1 to 8
      call prt(str$(counter) + ".  ")
      temp$ = string$(25,32)
      if typename$(counter) = ""_
         then lset temp$ = "[Not named]"_
         else lset temp$ = typename$(counter)
      if type(counter) = 1_
         then call prt(temp$ + "[activated]" + chr$(13))_
         else call prt(temp$ + "[deactivated]" + chr$(13))
   next counter
   goto bmmenu

bmnewcallers:
   call prt(chr$(13) + "System is now ")
   if closed_
      then closed = 0 : call prt("open ")_
      else closed = 1 : call prt("closed ")
   call prt("to new callers." + chr$(13))
   goto bmmenu

bmratio:
   call inpt(chr$(13) + "Enter new download ratio." + chr$(13) + ": ",_
      value$,3)
   if fntruenum(value$) = 0 or value$ = "" then bmmenu
   value = val(value$)
   if value < 0_
      then call prt(chr$(13) + "Invalid download ratio value." + chr$(13))_
      else uldlratio = value
   goto bmmenu

bmrename:
   call prt(chr$(13) + "Rename user type." + chr$(13))
   call inpt(chr$(13) + "Which type [1 - 8] ",value$,1)
   if value$ = "" then bmmenu
   value = val(value$)
   if value < 1 or value > 8_
      then call prt("Invalid user type." + chr$(13)) : goto bmmenu
   call prt(chr$(13) + typename$(value) + " selected." + chr$(13))
   call inpt(chr$(13) + "Rename it to: ",tempname$,25)
   if tempname$ = "" then bmmenu
   if tempname$ = " " then tempname$ = ""
   typename$(value) = tempname$
   goto bmmenu

bmtimeout:
   call inpt(chr$(13) + "Enter new time-out value (in seconds)." +_
      chr$(13) + ": ",value$,3)
   if fntruenum(value$) = 0 or value$ = "" then bmmenu
   value = val(value$)
   if (value = 0 and instr(value$,"0") = 0) or value < 10_
      then call prt("Invalid time-out value." + chr$(13)) : goto bmmenu
   timeout = value
   goto bmmenu

bmunvaldflt:
   call prt(chr$(13) + "Unvalidated user defaults." + chr$(13) + chr$(13))
   do
      uc = 1
      call inpt("Auto-feedback for new users? (y/n) ",value$,0)
   loop until value$ = "Y" or value$ = "N" or value$ = ""
   if value$ = "" then bmmenu
   if value$ = "Y"_
      then dfltunvallevel = (dfltunvallevel or 128)_
      else dfltunvallevel = (dftlunvallevel and 127)
   call inpt("Enter default time limit: ",value$,3)
   if fntruenum(value$) = 0 then bmmenu
   if value$ = "" then bmmenu
   value = val(value$)
   if value < 1 or value > 255_
      then call prt("Invalid time limit." + chr$(13)) : goto bmmenu
   temptime = value
   call inpt("Enter default security level: ",value$,2)
   if value$ = "" then bmmenu
   value = val(value$)
   if value < 0 or value > 15_
      then call prt("Invalid security level." + chr$(13)) : goto bmmenu
   templevel = value
   tempmap = 0 : value$ = ""
   for counter = 1 to 8
      if (type(counter) = 0) then bmunvalnext
      bmunvalprompt:
      uc = 1
      call inpt(typename$(counter) + " (y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ <> "Y" and value$ <> "N" then bmunvalprompt
      if value$ = "Y"_
         then tempmap = tempmap or (2 ^ (counter - 1))_
         else tempmap = tempmap and (32767 - (2 ^ (counter - 1)))
   bmunvalnext:
   next counter
   if value$ = "Q" then bmmenu
   dfltunvaltime = temptime
   dfltunvallevel = dfltunvallevel or templevel
   dfltunvaltype = tempmap
   goto bmmenu

bmvaltime:
   call inpt(chr$(13) + "Enter default time limit for validated " +_
      "users." + chr$(13) + ": ",value$,3)
   if fntruenum(value$) = 0 then bmmenu
   if value$ = "" then bmmenu
   value = val(value$)
   if value < 1 or value > 255_
      then call prt("Invalid time limit." + chr$(13)) : goto bmmenu
   dfltvaltime = value
   goto bmmenu

bmrecon:
   call prt(chr$(13) + "Counting users...")
   counter = 0 : totalusers = 0
   do
      incr counter
      get #2, counter
      if eof(2) then exit loop
      call unpad(username$,tempname$)
      if left$(tempname$,1) <> chr$(1) then incr totalusers
   loop
   call prt(str$(totalusers) + " users found." + chr$(13))
   goto bmmenu

'--------------------
fileeditor:
   call inpt(chr$(13) + "Enter filename: ",filenm$,12)
   if filenm$ = "" then mainmenu
   if fntruefile(filenm$) = 0_
      then call prt(chr$(13) + "Invalid filename format." + chr$(13)) :_
           goto mainmenu
   title$ = chr$(1)
   call editor(filenm$,50)
   goto mainmenu
