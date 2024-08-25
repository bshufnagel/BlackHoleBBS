'Black Hole BBS new user routines
'by Brad Hufnagel
'----------------------------------------------------------------------------

newuser:
   if closed or totalusers = 500_
      then call fileprt(bbspfx$ + "TEXT\CLOSED.MSG",0) :_
           call addhistory("[Closed]") :_
           goto logoff
   call addhistory("[New user: ")
   gosub termcnf
   call fileprt(bbspfx$ + "TEXT\NEWUSER1.MSG",0)
   newname:
     uc = 1
     call inpt(chr$(13) + chr$(13) + "Enter your real first and last name." +_
        chr$(13) + ": ",realname$,20)
     if realname$ = "" then newname
     call prt(chr$(13) + "Searching user base..." + chr$(13))
     counter = 0 : badname = 0
     do
        incr counter
        get #2, counter
        call unpad(username$,tempname$)
        if tempname$ = realname$ then badname = 1
     loop until eof(2) or badname = 1
     if badname = 1_
        then call prt(chr$(13) + "You're already on the system!" + chr$(13)) :_
             call addhistory("[Already on]" + chr$(13) + chr$(10) +_
                fnhistdate$ + " " + time$ + " ") :_
             goto enterpw
   newaddress:
     uc = 1
     call inpt(chr$(13) + "Enter your street address." + chr$(13) +_
        ": ",address$,22)
     if address$ = "" then newaddress
   newcity:
     uc = 1
     call inpt(chr$(13) + "Enter your city, state, and ZIP code." +_
        chr$(13) + ": ",city$,22)
     if city$ = "" then newcity
   newphone:
     uc = 1
     call inpt(chr$(13) + "Enter your phone number in the form " +_
        "314-555-1212." + chr$(13) + ": ",newphone$,12)
     phone$ = ""
     for counter = 1 to len(newphone$)
        charval = asc(mid$(newphone$,counter,1))
        if charval >= 48 and charval <= 57_
           then phone$ = phone$ + chr$(charval)
     next counter
     if len(phone$) < 10_
        then call prt("Invalid phone number, try again." + chr$(13)) :_
             goto newphone
   newhandle:
      uc = 1
      call inpt(chr$(13) + "If you'd like to use a handle on here, " +_
         "enter it now, or else just hit [Cr]." + chr$(13) + ": ",handle$,20)
      if handle$ = "" then handle$ = realname$ : goto newpassword
      call prt(chr$(13) + "Checking for duplicate handles..." + chr$(13))
      counter = 0 : badhandle = 0
      do
         incr counter
         get #2, counter
         call unpad(userhandle$,temphandle$)
         if temphandle$ = handle$ then badhandle = 1
      loop until eof(2) or badhandle = 1
      if badhandle_
         then call prt(chr$(13) + "That handle is already in use, " +_
              "try again." + chr$(13)) :__
              goto newhandle
   newpassword:
      uc = 1
      call inpt(chr$(13) + "Enter a password up to 8 characters " +_
         "long." + chr$(13) + ": ",password$,8)
      if password$ = "" then newpassword
   call prt(chr$(13) + "Let's see if I've got this right.  You are:" +_
      chr$(13) + chr$(13))
   call prt("              " + realname$ + chr$(13))
   call prt("    living at " + address$ + chr$(13))
   call prt("              " + city$ + chr$(13))
   call prt("Your phone is (" + left$(phone$,3) + ") " + mid$(phone$,4,3)_
      + "-" + right$(phone$,4) + chr$(13) + chr$(13))
   if realname$ <> handle$_
      then call prt("You will be known as " + handle$ + " on this system." +_
           chr$(13))
   call prt("Your password you will use for logon is " + password$ +_
      "." + chr$(13))
   newcheck:
      uc = 1
      call inpt(chr$(13) + "Is this correct? (y/n) ",check$,0)
      if check$ = "N" then newname
      if check$ <> "Y" then newcheck
   call addhistory(realname$ + " " + address$ + " " + city$ + "  Phone: "_
      + phone$ + "]" + chr$(13) + chr$(10) + fnhistdate$ + " " + time$ + " ")
   userid = 0
   do
      userid = userid + 1
      get #2, userid
      if left$(username$,1) = chr$(1) then exit loop
   loop until eof(2)
   tempid$ = right$(str$(userid),len(str$(userid)) - 1)
   call prt(chr$(13) + "Your user ID # is " + tempid$ + "." + chr$(13))
   seclevel = (dfltunvallevel and 15)
   timeallowed = dfltunvaltime
   timeontoday = 0
   totalcalls = 0
   currentdate$ = date$
   firstcall$ = chr$(val(left$(currentdate$,2)))_
      + chr$(val(mid$(currentdate$,4,2))) + chr$(val(right$(currentdate$,2)))
   lastcalldate$ = firstcall$ + chr$(val(left$(time$,2)))_
      + chr$(val(mid$(time$,4,2)))
   lastmsgptr = 0
   usertype = dfltunvaltype
   filter$ = string$(6,0)
   uploadtotal& = 0
   downloadtotal& = 0
   totalusers = totalusers + 1
   lset username$ = realname$
   lset userhandle$ = handle$
   lset useraddress$ = address$
   lset usercity$ = city$
   lset userphone$ = phone$
   lset userpassword$ = password$
   lset userseclevel$ = chr$(seclevel)
   lset usertimeallowed$ = chr$(timeallowed)
   lset usertimeontoday$ = chr$(timeontoday)
   lset usertotalcalls$ = mki$(totalcalls)
   lset userfirstcall$ = firstcall$
   lset userlastcall$ = lastcalldate$
   lset userlastmsgptr$ = mki$(0)
   lset usertype$ = chr$(usertype)
   lset uservideo$ = chr$(videowidth)
   lset usertermstatus$ = chr$(termstatus)
   lset userfilter$ = string$(6,0)
   lset userupload$ = mkl$(uploadtotal&)
   lset userdownload$ = mkl$(downloadtotal&)
   put #2, userid
   call fileprt(bbspfx$ + "TEXT\NEWUSER2.MSG",0)
   if (dfltunvallevel and 128) = 128_
      then emailid = 1 :_
           emailtitle$ = "Auto Feedback" :_
           gosub emailchkid
   get #2, userid
   goto welcparms
