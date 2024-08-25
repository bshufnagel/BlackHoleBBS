'Black Hole BBS email functions
'by Brad Hufnagel
'----------------------------------------------------------------------------

email:
   menucmd$ = "FHQRS"
   menu$ = "F Feedback to sysop\H Help\Q Quit\R Receive mail\S Send mail\"
   call printmenu("Electronic mail")
   selection = instr(menucmd$,cmd$)
   call addhistory(lcase$(cmd$))
   on selection goto emailfeedback, emailhelp, mainmenu, emailreceive,_
      emailsend

emailfeedback:
   emailid = 1
   emailtitle$ = "Feedback"
   call prt(chr$(13) + "Feedback to Sysop" + chr$(13))
   gosub emailchkid
   goto email

emailhelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\EMAIL.HLP",0)
   goto email

emailreceive:
   email(0) = 0
   get #7, 256
   emailtotal = cvi(emailnum$)
   if emailtotal = 0 then emailnone
   for counter = 1 to emailtotal
      get #7, counter
      if cvi(emailrecv$) = userid_
         then incr email(0) :_
              email(email(0)) = counter
   next counter
   emailnone:
   if email(0) = 0_
      then call prt(chr$(13) + "No mail waiting." + chr$(13)) :_
           goto email
   readloc = 1

emailread:
   get #7, email(readloc)
   msg$ = str$(cvi(emailnum$))
   filenm$ = bbspfx$ + "EMAIL\" + right$(msg$,len(msg$) - 1) + ".EML"
   readend = email(0)
   call fileprt(filenm$,readloc)

emailprompt:
   menucmd$ = "AHJKNPRQ"
   if seclevel > 6 then menucmd$ = menucmd$ + "U"
   menu$ = "Auto-reply\Help\Jump\Next\Previous\Quit\Reread\User edit *\"
   if readloc = readend_
      then call printmenu("Last Email")_
      else call printmenu("Email")
   selection = instr(menucmd$,cmd$)
   if cmd$ = chr$(13) then emailnext
   on selection goto emailauto, emailreadhelp, emailjump, emailkill,_
      emailnext, emailprev, emailread, emailquit, emailedit

emailauto:
   emailid = msgid
   emailtitle$ = msgtitle$
   get #2, emailid
   call unpad(username$,emailname$)
   call unpad(userhandle$,emailhandle$)
   if emailname$ = chr$(1) or emailname$ <> msgname$_
      or (usertype and asc(usertype$)) = 0_
      then call prt(chr$(13) + "User has been deleted!" + chr$(13)) :_
           goto emailprompt
   uc = 1
   call inpt(chr$(13) + "Send to " + emailhandle$ + "? (y/n) ",value$,0)
   if value$ <> "Y" then emailprompt
   gosub emailentry
   if fnfileexist(filenm$) = 0 then emailprompt
   tempnm$ = filenm$
   get #7, email(readloc)
   msg$ = str$(cvi(emailnum$))
   filenm$ = bbspfx$ + "EMAIL\" + right$(msg$,len(msg$) - 1) + ".EML"
   if fnfileexist(filenm$) = 1 then kill filenm$
   get #7, 256
   value = cvi(emailnum$)
   decr value
   lset emailnum$ = mki$(value)
   put #7, 256
   if email(readloc) <> emaillimit_
      then for counter = email(readloc) + 1 to emaillimit :_
              get #7, counter :_
              put #7, counter - 1 :_
           next counter
   if readloc <> email(0)_
      then for counter = readloc to email(0) - 1 :_
              email(counter) = email(counter + 1) - 1 :_
           next counter
   decr email(0)
   decr readloc
   filenm$ = tempnm$
   gosub emailsave
   goto emailnext

emailedit:
  searchlist$ = "E"
  editid = msgid
  gosub edituser
  goto emailprompt

emailreadhelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\EMAILRD.HLP",0)
   goto emailprompt

emailjump:
   readstart = 1 : readend = email(0)
   call prt(chr$(13) + "Email from #" + str$(readstart) + " to #"_
      + str$(readend) + chr$(13))
   call inpt(chr$(13) + "Enter number to jump to: ",email$,5)
   if fntruenum(email$) = 0 then emailprompt
   jumpvalue = val(email$)
   if jumpvalue >= readstart and jumpvalue <= readend_
      then readloc = jumpvalue :_
           goto emailread
   if jumpvalue > readend_
      then call prt(chr$(13) + "No more mail." + chr$(13)) : goto emailquit
   goto emailprompt

emailkill:
   get #7, email(readloc)
   msg$ = str$(cvi(emailnum$))
   filenm$ = bbspfx$ + "EMAIL\" + right$(msg$,len(msg$) - 1) + ".EML"
   if fnfileexist(filenm$) = 1 then kill filenm$
   get #7, 256
   value = cvi(emailnum$)
   decr value
   lset emailnum$ = mki$(value)
   put #7, 256
   if email(readloc) <> emaillimit_
      then for counter = email(readloc) + 1 to emaillimit :_
              get #7, counter :_
              put #7, counter - 1 :_
           next counter
   if readloc <> email(0)_
      then for counter = readloc to email(0) - 1 :_
              email(counter) = email(counter + 1) - 1 :_
           next counter
   decr email(0)
   decr readloc
   goto emailnext

emailnext:
   incr readloc
   if readloc > email(0)_
      then call prt(chr$(13) + "No more mail." + chr$(13)) :_
           goto emailquit
   goto emailread

emailprev:
   decr readloc
   if readloc = 0_
      then call prt(chr$(13) + "Beginning of mail." + chr$(13)) :_
           incr readloc :_
           goto emailprompt
   goto emailread

emailquit:
   if email = 0_
      then email_
      else email = 0 : goto mainmenu

emailsend:
   if seclevel = 0_
      then emailid = 1 :_
           emailtitle$ = "Mail" :_
           call prt(chr$(13) + "Mail will automatically be directed to " +_
              "Sysop until you are validated." + chr$(13)) :_
           gosub emailchkid :_
           goto email
   call inpt(chr$(13) + "Enter the name, handle, or user ID # of the " +_
      "receiver." + chr$(13) + ": ",value$,20)
   if value$ = "" then email
   call unpad(value$,email$)
   if fntruenum(email$) = 0_
      then emailid = 0_
      else emailid = val(email$)
   if emailid > 0_
      then if emailid <> 1_
         then gosub emailchkid : goto email_
         else call prt(chr$(13) + "Email to the sysop should be sent " +_
                 "using the Feedback option." + chr$(13)) :_
              goto email
   emailid = 0 : found = 0
   do
      incr emailid
      get #2, emailid
      call unpad(username$,tempname$)
      call unpad(userhandle$,temphandle$)
      if tempname$ <> email$_
         and temphandle$ <> email$_
         then emailsrchloop
      if (usertype and asc(usertype$)) <> 0 then found = 1
   emailsrchloop:
   loop until eof(2) or found = 1
   if found = 0 then call prt(chr$(13) + "User not found." + chr$(13)) :_
      goto email
   if emailid = 1 then call prt(chr$(13) + "Email to the sysop should be " +_
      "sent using the Feedback option." + chr$(13)) : goto email
   call unpad(userhandle$,emailhandle$)
   emailtitle$ = "Mail" : gosub emailentry : gosub emailsave : goto email

emailchkid:
   incr nest
   chkid& = emailid
   if (chkid& * 134) > lof(2)_
      then call prt(chr$(13) + "Invalid user number." + chr$(13)) :_
              nest = nest - 1 : return
   get #2, emailid
   call unpad(username$,emailname$)
   call unpad(userhandle$,emailhandle$)
   if emailname$ = chr$(1) or (usertype and asc(usertype$)) = 0_
      then call prt(chr$(13) + "Invalid user number." + chr$(13)) :_
              nest = nest - 1 : return
   if emailid = 1_
      then gosub emailentry : gosub emailsave : decr nest : return
   uc = 1
   call inpt(chr$(13) + "Send to " + emailhandle$ + "? (y/n) ",value$,0)
   if value$ = "Y"_
      then emailtitle$ = "Mail" : gosub emailentry : gosub emailsave
   decr nest
   return

emailentry:
   incr nest
   msg$ = str$(msgptr + 1)
   filenm$ = bbspfx$ + "EMAIL\" + right$(msg$,len(msg$) - 1) + ".EML"
   title$ = emailtitle$
   call editor(filenm$,50)
   decr nest
   return

emailsave:
   incr nest
   if fnfileexist(filenm$) = 0 then decr nest : return
   incr msgptr
   get #7, 256
   value = cvi(emailnum$)
   if value = emaillimit_
      then decr value :_
           lset emailnum$ = mki$(value) :_
           put #7, 256 :_
           get #7, 1 :_
           msg$ = str$(cvi(emailnum$)) :_
           filenm$ = bbspfx$ + "EMAIL\" + right$(msg$,len(msg$) - 1)_
              + ".EML" :_
           if fnfileexist(filenm$) = 1 then kill filenm$ :_
           for counter = 2 to emaillimit :_
              get #7, counter :_
              put #7, counter - 1 :_
           next counter
   get #7, 256
   emailvalue = cvi(emailnum$)
   incr emailvalue
   lset emailnum$ = mki$(emailvalue)
   put #7, 256
   lset emailnum$ = mki$(msgptr)
   lset emailrecv$ = mki$(emailid)
   put #7, emailvalue
   if emailid = userid_
      then incr email(0) :_
           email(email(0)) = emailvalue
   msgactivity = 1
   if emailtitle$ <> "Auto Feedback" then call addhistory("+")
   call prt(chr$(13) + "Email sent to " + emailhandle$ + "." + chr$(13))
   decr nest
   return
