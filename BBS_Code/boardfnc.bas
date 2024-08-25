'Black Hole BBS board functions
'by Brad Hufnagel
'----------------------------------------------------------------------------

boardfunctions:
   menucmd$ = "AEFHLKPRQU"
   if seclevel >= 3 then menucmd$ = menucmd$ + "MZC"
   if seclevel >= 9 then menucmd$ = menucmd$ + "S"
   menu$ = "A All new messages\C Change subboard configuration\E Expanded subboard listing\F Filter subboards\H Help\K Kill message\L List subboards\M Make subboard\P Post a message\Q Quit\R Read messages\" +_
      "S Subboard size change *\U Unfilter all subboards\Z Zap subboard\"
   call printmenu("Board functions")
   selection = instr(menucmd$,cmd$)
   call addhistory(lcase$(cmd$))
   on selection goto boardallnew, expandedlist, filterboards,_
      boardhelp, listboards, killmessage, postmessage, readmessage, mainmenu,_
      unfilterboards, makesubboard, zapboard, reconfigboard, boardsize

boardallnew:
   gosub allnewmsgs
   goto boardfunctions

allnewmsgs:
   allnew = 1 : nest = nest + 1 : readcont = 0
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           nest = nest - 1 : return
   msgactivity = 1 : new = 0
   if allnewsub = 0_
      then allnewsub = 1_
      else uc = 1 :_
           call inpt(chr$(13) + "Restart from beginning? (y/n) ",value$,0) :_
           if value$ = "Y" then allnewsub = 1
   for readboard = allnewsub to boardlist(0)
      if fnfiltered(boardlist(readboard)) = 1 then allcont
      goodmap = 0
      value = (systype and boardread(boardlist(readboard))) and usertype
      if seclevel > 8 then goodmap = 1
      for counter = 1 to 8
      if value and (2 ^ (counter - 1)) = (2 ^ (counter - 1))_
         and type(counter) = 1_
         then goodmap = 1
      next counter
      if goodmap = 0 then allcont
      get #6, 256 * boardlist(readboard)
      readstart = 1 : readend = cvi(msgnum$)
      if readend = 0 then allcont
      get #6, (256 * boardlist(readboard)) - 256 + readend
      if lastmsgptr >= cvi(msgnum$) then allcont
      for counter = 1 to readend
         get #6, (256 * boardlist(readboard)) - 256 + counter
         if lastmsgptr < cvi(msgnum$)_
            then readloc = counter : exit for
      next counter
      newmsgs = readend - readloc + 1
      new = 1
      call prt(chr$(13) + "-- " + boardname$(boardlist(readboard)) +_
         ".  Board #" + str$(readboard) + "," + str$(newmsgs) +_
         " new msgs." + chr$(13))
      allread:
      nest = nest + 1
      gosub readmsg
      if cmd$ = "X" then allcont
      if cmd$ = "Q" then exit for
      allstop:
      uc = 1
      call prt(chr$(13) + "End of " + boardname$(boardlist(readboard)) + ".")
      if ((boardpost(boardlist(readboard)) and usertype) = 0)_
         and (seclevel < 9)_
         then call prt(chr$(13)) : goto allcont
      if expert_
         then call inpt("  Add (y/n/r/q) ",cmd$,0)_
         else call inpt("  Would you like to add to it? (y/n/r/q) ",cmd$,0)
      if cmd$ = "Y" then postboard = readboard : gosub postmsg_
         else if cmd$ = "N" or cmd$ = chr$(13) then allcont_
         else if cmd$ = "R" then readloc = readend : readcont = 0 :_
                 goto allread_
         else if cmd$ = "Q" then allnewsub = readboard : exit for_
         else allstop
   allcont:
   next readboard
   if new = 0_
      then allnewsub = 0 :_
           call prt(chr$(13) + "No new messages posted." + chr$(13))_
      else if cmd$ <> "Q"_
         then allnewsub = 0 :_
              call prt(chr$(13) + "No more messages." + chr$(13))
   allbye:
   nest = nest - 1
   return

boardsize:
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   call prt(chr$(13) + "Subboard size change." + chr$(13))
   call inpt(chr$(13) + "Which board [1 -" + str$(boardlist(0)) +_
      "] ",value$,2)
   if value$ = "" then boardfunctions
   sizeboard = val(value$)
   if sizeboard < 1 or sizeboard > boardlist(0) then boardfunctions
   call prt(chr$(13) + boardname$(boardlist(sizeboard)) + " selected." +_
      chr$(13))
   call prt(chr$(13) + "Current size is" +_
      str$(boardlimit(boardlist(sizeboard))) + " messages." + chr$(13))
   uc = 1 : call inpt("Do you want to change this? (y/n) ",test$,0)
   if test$ <> "Y" then boardfunctions
   call inpt(chr$(13) + "Enter new size: ",value$,3)
   if fntruenum(value$) = 0 then boardfunctions
   limit = val(value$)
   if limit < 10 or limit > 255_
      then call prt(chr$(13) + "Invalid value." + chr$(13)) :_
           goto boardfunctions
   if limit = boardlimit(boardlist(sizeboard))_
      then call prt(chr$(13) + "Size remains unchanged." + chr$(13)) :_
           goto boardfunctions
   get #6, 256 * boardlist(sizeboard)
   if (limit > boardlimit(boardlist(sizeboard))) or (limit >= cvi(msgnum$))_
      then sizeend
   call prt(chr$(13) + "Deleting old messages...")
   sizestart = cvi(msgnum$) - limit
   for counter = 1 to sizestart
      get #6, (256 * boardlist(sizeboard)) - 256 + counter
      brdnum$ = str$(boardlist(sizeboard))
      msg$ = str$(cvi(msgnum$))
      filenm$ = bbspfx$ + "MSGS\BOARD_" + right$(brdnum$,len(brdnum$) - 1) +_
         "\" + right$(msg$,len(msg$) - 1) + ".MSG"
      if fnfileexist(filenm$) = 1 then kill filenm$
   next counter
   get #6, (256 * boardlist(sizeboard))
   value = cvi(msgnum$)
   value = value - sizestart
   lset msgnum$ = mki$(value)
   put #6, (256 * boardlist(sizeboard))
   for counter = sizestart + 1 to boardlimit(boardlist(sizeboard))
      get #6, (256 * boardlist(sizeboard)) - 256 + counter
      put #6, (256 * boardlist(sizeboard)) - 256 + (counter - sizestart)
   next counter
   call prt("done!" + chr$(13))
   sizeend:
   boardlimit(boardlist(sizeboard)) = limit
   call prt(chr$(13) + "Size is now" + str$(limit) + " messages." + chr$(13))
   goto boardfunctions

reconfigboard:
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   if (systype and usertype) = 0_
      then call prt(chr$(13) + "You don't have the proper user types to " +_
              "reconfigure a subboard." + chr$(13)) :_
           goto boardfunctions
   call inpt(chr$(13) + "Reconfigure which subboard [1 -" +_
      str$(boardlist(0)) + "] ",reconfignum$,2)
   reconfignum = val(reconfignum$)
   if reconfignum < 1 or reconfignum > boardlist(0) then boardfunctions
   if (boardowner(boardlist(reconfignum)) <> userid) and (seclevel < 7)_
      then call prt(chr$(13) + "That subboard isn't yours!" + chr$(13)) :_
           goto boardfunctions
   call prt(chr$(13) + boardname$(boardlist(reconfignum)) + " selected." +_
      chr$(13))
   call inpt(chr$(13) + "Enter the new name of your subboard." +_
      chr$(13) + ": ",tempname$,50)
   if tempname$ = "" then boardfunctions
   reconfigstats:
   postmap = 0 : readmap = 0
   for counter = 1 to 8
      if type(counter) = 0 then reconfignext
      if (usertype and (2 ^ (counter - 1))) = 0 then reconfignext
      reconfigreadchk:
      call prt(chr$(13))
      uc = 1
      call inpt("Can a " + typename$(counter) + " read your board? " +_
         "(y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ = "Y"_
         then readmap = readmap + (2 ^ (counter - 1))_
         else if value$ <> "N" then reconfigreadchk
      reconfigpostchk:
      uc = 1
      call inpt("Can a " + typename$(counter) + " post on your board? " +_
         "(y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ = "Y"_
         then postmap = postmap + (2 ^ (counter - 1))_
         else if value$ <> "N" then reconfigpostchk
   reconfignext:
   next counter
   if value$ = "Q" then boardfunctions
   if postmap = 0 and readmap = 0_
      then call prt(chr$(13) + "Someone has to be able to do SOMETHING " +_
              "on your board!" + chr$(13)) : goto reconfigstats
   tempid = boardowner(boardlist(reconfignum))
   if (seclevel > 6) and (tempid <> userid)_
      then uc = 1 :_
          call inpt(chr$(13) + "This subboard was not originally yours.  " +_
             "Do you want to be the owner? (y/n) ",value$,0) :_
          if value$ = "Y" then tempid = userid
   boardname$(boardlist(reconfignum)) = tempname$
   boardowner(boardlist(reconfignum)) = tempid
   boardpost(boardlist(reconfignum)) = postmap
   boardread(boardlist(reconfignum)) = readmap
   call prt(chr$(13) + "Your subboard is #" + str$(reconfignum) + "." +_
      chr$(13))
   goto boardfunctions

zapboard:
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   call inpt(chr$(13) + "Zap which subboard [1 -" + str$(boardlist(0)) +_
      "] ",deletenum$,2)
   deletenum = val(deletenum$)
   if deletenum < 1 or deletenum > boardlist(0) then boardfunctions
   if (boardowner(boardlist(deletenum)) <> userid) and (seclevel < 7)_
      then call prt(chr$(13) + "That subboard isn't yours!" + chr$(13)) :_
           goto boardfunctions
   call prt(chr$(13) + boardname$(boardlist(deletenum)) + " selected." +_
      chr$(13))
   uc = 1 : call inpt(chr$(13) + "Are you sure? (y/n) ",value$,0)
   if value$ <> "Y" then boardfunctions
   call prt(chr$(13) + "Deleting "  + boardname$(boardlist(deletenum)) +_
      "." + chr$(13))
   brdnum$ = str$(boardlist(deletenum))
   get #6, 256 * boardlist(deletenum)
   totalmsgs = cvi(msgnum$)
   if totalmsgs = 0 then deletecont
   for counter = 1 to totalmsgs
      get #6, (256 * boardlist(deletenum)) - 256 + counter
      msg$ = str$(cvi(msgnum$))
      filenm$ = bbspfx$ + "MSGS\BOARD_" + right$(brdnum$,len(brdnum$) - 1) +_
         "\" + right$(msg$,len(msg$) - 1) + ".MSG"
      if fnfileexist(filenm$) = 1 then kill filenm$
   next counter
   deletecont:
   filenm$ = bbspfx$ + "MSGS\BOARD_" + right$(brdnum$,len(brdnum$) - 1)
   rmdir filenm$
   lset msgnum$ = mki$(0)
   put #6, 256 * boardlist(deletenum)
   boardname$(boardlist(deletenum)) = ""
   boardowner(boardlist(deletenum)) = 0
   boardlist(deletenum) = 0
   totalsubboards = totalsubboards - 1
   call setboardlist
   call prt(chr$(13) + "Deletion completed." + chr$(13))
   goto boardfunctions

expandedlist:
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   call inpt(chr$(13) + "Start listing at [1 -" + str$(boardlist(0)) +_
      "] ",start$,2)
   start = val(start$)
   if start$ = "" then start = 1
   if start < 1 or start > boardlist(0) then boardfunctions
   abortable = 1
   call prt(chr$(13))
   for counter = start to boardlist(0)
      get #2, boardowner(boardlist(counter))
      call unpad(userhandle$,maker$)
      get #6, 256 * boardlist(counter)
      expandedcont:
      if fnfiltered(boardlist(counter)) = 1_
         then call prt("-")_
         else call prt(" ")
      if aborted then exit for
      if counter < 10 then call prt(" ")
      call prt(str$(counter) + ". " + boardname$(boardlist(counter)) +_
         " [" + maker$ + "] ")
      if aborted then exit for
      call prt(str$(cvi(msgnum$)) + " msgs")
      if aborted then exit for
      if seclevel >= 9_
         then call prt("," + str$(boardlimit(boardlist(counter))) +_
                 " msg capacity" + chr$(13))_
         else call prt(chr$(13))
      if aborted then exit for
      call prt("          Read: ")
      if aborted then exit for
      start = 0
      for counter2 = 1 to 8
         if type(counter2) = 0 then expandreadnext
         if ((boardread(boardlist(counter)) and 2 ^ (counter2 - 1))_
            = (2 ^ (counter2 - 1))) and ((usertype and 2 ^ (counter2 - 1))_
            = (2 ^ (counter2 - 1)))_
            then if start = 0_
                    then start = 1 : call prt(typename$(counter2))_
                    else call prt(", " + typename$(counter2))
         if aborted then exit for
      expandreadnext:
      next counter2
      if aborted then exit for
      call prt(chr$(13) + "          Post: ")
      if aborted then exit for
      start = 0
      for counter2 = 1 to 8
         if type(counter2) = 0 then expandpostnext
         if ((boardpost(boardlist(counter)) and 2 ^ (counter2 - 1))_
            = (2 ^ (counter2 - 1))) and ((usertype and 2 ^ (counter2 - 1))_
            = (2 ^ (counter2 - 1)))_
            then if start = 0_
                    then start = 1 : call prt(typename$(counter2))_
                    else call prt(", " + typename$(counter2))
         if aborted then exit for
      expandpostnext:
      next counter2
      call prt(chr$(13) + chr$(13))
      if aborted then exit for
   next counter
   if aborted then boardfunctions
   call prt("- indicates filtered" + chr$(13))
   abortable = 0
   goto boardfunctions

filterboards:
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   call prt(chr$(13) + "Filter/unfilter subboards." + chr$(13))
   call inpt(chr$(13) + "Which board [1 -" + str$(boardlist(0)) +_
      "] ",value$,2)
   if value$ = "" then boardfunctions
   filterbrd = val(value$)
   if filterbrd < 1 or filterbrd > boardlist(0) then boardfunctions
   call prt(chr$(13) + boardname$(boardlist(filterbrd)) + " selected." +_
      chr$(13))
   call prt(chr$(13) + "Current status is ")
   filterstatus = fnfiltered(boardlist(filterbrd))
   if filterstatus = 0_
      then call prt("unfiltered." + chr$(13))_
      else call prt("filtered." + chr$(13))
   uc = 1 : call inpt("Do you want to change this? (y/n) ",test$,0)
   if test$ <> "Y"_
      then call prt(chr$(13) + "Status unchanged." + chr$(13)) :_
           goto boardfunctions
   if filterstatus = 0_
      then mid$(filter$,filterbyte,1) = chr$(asc(mid$(filter$,filterbyte,1))_
           + 2 ^ filterbit) :_
           call prt(chr$(13) + boardname$(boardlist(filterbrd))_
                    + " is now filtered." + chr$(13)) :_
           goto boardfunctions_
      else mid$(filter$,filterbyte,1) = chr$(asc(mid$(filter$,filterbyte,1))_
           - 2 ^ filterbit) :_
           call prt(chr$(13) + boardname$(boardlist(filterbrd))_
                   + " is now unfiltered." + chr$(13)) :_
           goto boardfunctions

boardhelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\BOARD.HLP",0)
   goto boardfunctions

killmessage:
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   call inpt(chr$(13) + "Which board? [1 -" + str$(boardlist(0)) +_
      "] ",value$,2)
   if value$ = "" then boardfunctions
   killbrd = val(value$)
   if killbrd < 1 or killbrd > boardlist(0) then boardfunctions
   call prt(chr$(13) + boardname$(boardlist(killbrd)) + " selected." +_
      chr$(13))
   call inpt(chr$(13) + "Enter message # to kill: ",msg$,5)
   if fntruenum(msg$) = 0 then boardfunctions
   deletenum = val(msg$)
   if deletenum = 0 or deletenum > 255 then boardfunctions
   uc = 1
   call inpt(chr$(13) + "Are you sure? (y/n) ",value$,0)
   if value$ <> "Y" then boardfunctions
   get #6, (256 * boardlist(killbrd)) - 256 + deletenum
   brdnum$ = str$(boardlist(killbrd))
   msg$ = str$(cvi(msgnum$))
   if val(msg$) = 0_
      then call prt(chr$(13) + "Message not found." + chr$(13)) :_
           goto boardfunctions
   filenm$ = bbspfx$ + "MSGS\BOARD_" + right$(brdnum$,len(brdnum$) - 1) +_
      "\" + right$(msg$,len(msg$) - 1) + ".MSG"
   open filenm$ for input as #4
      input #4, msgid
   close #4
   if (msgid <> userid) and (seclevel < 7)_
      then call prt(chr$(13) + "That is not your message!" + chr$(13)) :_
           goto boardfunctions
   if fnfileexist(filenm$) = 1 then kill filenm$
   if deletenum <> 255_
      then for counter = deletenum + 1 to 255 :_
              get #6, (256 * boardlist(killbrd)) - 256 + counter :_
              put #6, (256 * boardlist(killbrd)) - 256 + counter - 1 :_
           next counter
   call prt(chr$(13) + "Message deleted." + chr$(13))
   goto boardfunctions

listboards:
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   endboard = 1
   for counter = 1 to boardlist(0)
   if (fnfiltered(boardlist(counter)) = 0)_
      and (boardlist(counter) > endboard)_
      then endboard = counter
   next counter
   call inpt(chr$(13) + "Start listing at [1 -" + str$(endboard) +_
      "] ",start$,2)
   start = val(start$)
   if start$ = "" then start = 1
   if start < 1 or start > endboard then boardfunctions
   call prt(chr$(13))
   abortable = 1
   for counter = start to endboard
      if fnfiltered(boardlist(counter)) <> 0 then listnext
      newmsgs = 0
      if msg(0,1) = 0 then listnewnext
      for msgchk = 1 to msg(0,1)
         if (msg(msgchk,1) = boardlist(counter))_
            and (msg(msgchk,2) > lastmsgptr)_
            then incr newmsgs
      next msgchk
      listnewnext:
      if counter < 10 then call prt(" ")
      if aborted then exit for
      call prt(str$(counter) + ". " + boardname$(boardlist(counter)))
      if aborted then exit for
      if newmsgs > 0_
         then call prt(" " + str$(newmsgs) + " new")
      if aborted then exit for
      call prt(chr$(13))
      if aborted then exit for
listnext:
   next counter
   abortable = 0
   goto boardfunctions

makesubboard:
   multiple = 0
   for counter = 1 to totalsubboards
      if boardowner(counter) = userid_
         then multiple = 1
   next counter
   if seclevel = 3 and multiple_
      then call prt(chr$(13) + "You already have a subboard." +_
              chr$(13)) :_
           goto boardfunctions
   if (systype and usertype) = 0_
      then call prt(chr$(13) + "You don't have the proper user types " +_
              "to make a subboard." + chr$(13)) :_
           goto boardfunctions
   if totalsubboards = 48 then_
      call prt(chr$(13) + "Sorry, but the maximum subboard limit has been " +_
         "reached." + chr$(13) + chr$(13)) :_
      goto boardfunctions
   uc = 0
   call inpt(chr$(13) + "Enter the name of your new subboard." + chr$(13) +_
      ": ",subboardname$,50)
   if subboardname$ = "" then boardfunctions
   makestats:
   postmap = 0 : readmap = 0
   for counter = 1 to 8
      if type(counter) = 0 then makenext
      if (usertype and (2 ^ (counter - 1))) = 0 then makenext
      makereadchk:
      call prt(chr$(13))
      uc = 1
      call inpt("Can a " + typename$(counter) + " read your board? " +_
         "(y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ = "Y"_
         then readmap = readmap + (2 ^ (counter - 1))_
         else if value$ <> "N" then makereadchk
      makepostchk:
      uc = 1
      call inpt("Can a " + typename$(counter) + " post on your board? " +_
         "(y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ = "Y"_
         then postmap = postmap + (2 ^ (counter - 1))_
         else if value$ <> "N" then makepostchk
   makenext:
   next counter
   if value$ = "Q" then boardfunctions
   if postmap = 0 and readmap = 0_
      then call prt(chr$(13) + "Someone has to be able to do SOMETHING " +_
              "on your board!" + chr$(13)) : goto makestats
   for counter = 1 to totalsubboards
      if boardname$(counter) = "" then exit for
   next counter
   boardname$(counter) = subboardname$
   boardowner(counter) = userid
   boardread(counter) = readmap
   boardpost(counter) = postmap
   boardptr(counter) = sysmsgptr
   boardlimit(counter) = 50
   totalsubboards = totalsubboards + 1
   call setboardlist
   for boardpos = 1 to totalsubboards
      if boardlist(boardpos) = counter then exit for
   next boardpos
   brdnum$ = str$(boardlist(boardpos))
   filenm$ = bbspfx$ + "MSGS\BOARD_" + right$(brdnum$,len(brdnum$) - 1)
   mkdir filenm$
   call prt(chr$(13) + "Your subboard is #" + str$(boardpos) + "." + chr$(13))
   goto boardfunctions

unfilterboards:
   uc = 1
   call inpt(chr$(13) + "Unfilter ALL subboards? (y/n) ",value$,0)
   if value$ <> "Y" then boardfunctions
   filter$ = string$(6,0)
   call prt(chr$(13) + "All subboards are now unfiltered." + chr$(13))
   goto boardfunctions

postmessage:
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   call inpt(chr$(13) + "Which board [1 -" + str$(boardlist(0)) +_
      "] ",board$,2)
   postboard = val(board$)
   if postboard < 1 or postboard > boardlist(0) then boardfunctions
   call prt(chr$(13) + boardname$(boardlist(postboard)) + " selected." +_
      chr$(13))
   goodmap = 0
   value = (systype and boardpost(boardlist(postboard))) and usertype
   if seclevel > 8 then goodmap = 1
   for counter = 1 to 8
      if value and (2 ^ (counter - 1)) = (2 ^ (counter - 1))_
         and type(counter) = 1_
         then goodmap = 1
   next counter
   if goodmap = 0_
      then call prt(chr$(13) + "You can't post on that board." + chr$(13)) :_
           goto boardfunctions
   gosub postmsg
   goto boardfunctions

postmsg:
   nest = nest + 1
   call inpt(chr$(13) + "Enter a title for your message:" + chr$(13) +_
      ": ",posttitle$,32)
   if posttitle$ = "" then nest = nest - 1 : return
   brdnum$ = str$(boardlist(postboard))
   msg$ = str$(msgptr + 1)
   filenm$ = bbspfx$ + "MSGS\BOARD_" + right$(brdnum$,len(brdnum$) - 1) +_
      "\" + right$(msg$,len(msg$) - 1) + ".MSG"
   title$ = posttitle$
   call editor(filenm$,50)
   if fnfileexist(filenm$) = 0 then nest = nest - 1 : return
   incr msgptr
   get #6, 256 * boardlist(postboard)
   postloc = cvi(msgnum$)
   if postloc = boardlimit(boardlist(postboard))_
      then get #6, (256 * boardlist(postboard)) - 256 + 1 :_
           brdnum$ = str$(boardlist(postboard)) :_
           msg$ = str$(cvi(msgnum$)) :_
           filenm$ = bbspfx$ + "MSGS\BOARD_" +_
              right$(brdnum$,len(brdnum$) - 1) + "\" +_
              right$(msg$,len(msg$) - 1) + ".MSG" :_
           if fnfileexist(filenm$) = 1 then kill filenm$ :_
           for postshift = 2 to 255 :_
              get #6, (256 * boardlist(postboard)) - 256 + postshift :_
              put #6, (256 * boardlist(postboard)) - 256 + postshift - 1 :_
           next postshift :_
           get #6, 256 * boardlist(postboard) :_
           value = cvi(msgnum$) :_
           decr value :_
           lset msgnum$ = mki$(value) :_
           put #6, 256 * boardlist(postboard) :_
           decr postloc
   incr postloc
   lset msgnum$ = mki$(msgptr)
   put #6, (256 * boardlist(postboard)) - 256 + postloc
   get #6, 256 * boardlist(postboard)
   value = cvi(msgnum$)
   incr value
   lset msgnum$ = mki$(value)
   put #6, 256 * boardlist(postboard)
   msgactivity = 1
   call addhistory("+")
   call prt(chr$(13) + "Message posted on " +_
      boardname$(boardlist(postboard)) + "." + chr$(13))
   nest = nest - 1
   return

readmessage:
   allnew = 0 : readcont = 0
   if boardlist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any subboards." +_
              chr$(13)) :_
           goto boardfunctions
   call inpt(chr$(13) + "Which board [1 -" + str$(boardlist(0)) +_
      "] ",board$,2)
   readboard = val(board$)
   if readboard < 1 or readboard > boardlist(0) then boardfunctions
   call prt(chr$(13) + boardname$(boardlist(readboard)) + " selected." +_
      chr$(13))
   goodmap = 0
   value = (systype and boardread(boardlist(readboard))) and usertype
   if seclevel > 8 then goodmap = 1
   for counter = 1 to 8
      if value and (2 ^ (counter - 1)) = (2 ^ (counter - 1))_
         and type(counter) = 1_
         then goodmap = 1
   next counter
   if goodmap = 0_
      then call prt(chr$(13) + "You can't read that board." + chr$(13)) :_
           goto boardfunctions
   get #6, 256 * boardlist(readboard)
   readstart = 1 : readend = cvi(msgnum$)
   if readend = 0_
      then call prt(chr$(13) + "No messages are posted." + chr$(13)) :_
           goto boardfunctions
   call prt(chr$(13) + "Messages from #" + str$(readstart) + " to #"_
      + str$(readend) + chr$(13))
   call inpt("Hit [Return] for messages since your last logon." +_
      chr$(13) + ": ",msg$,5)
   if fntruenum(msg$) = 0 and msg$ <> ""_
      then boardfunctions
   readloc = val(msg$)
   if readloc = 0 and msg$ <> "" then boardfunctions
   if readloc > 0 then readgo
   for counter = readstart to readend
      get #6, (256 * boardlist(readboard)) - 256 + counter
      if lastmsgptr < cvi(msgnum$)_
         then readloc = counter :_
              exit for
   next counter
   readgo:
   msgactivity = 1
   if readloc = 0 or readloc > readend_
      then call prt(chr$(13) + "No more messages." + chr$(13)) :_
           goto boardfunctions
   nest = nest + 1
   gosub readmsg
   goto boardfunctions

readmsg:
   get #6, (256 * boardlist(readboard)) - 256 + readloc
   brdnum$ = str$(boardlist(readboard))
   msg$ = str$(cvi(msgnum$))
   filenm$ = bbspfx$ + "MSGS\BOARD_" + right$(brdnum$,len(brdnum$) - 1) +_
      "\" + right$(msg$,len(msg$) - 1) + ".MSG"
   call fileprt(filenm$,readloc)
   if aborted then call prt("Cancel" + chr$(13)) : readcont = 0
   if readcont = 1 then readnext

readprompt:
   menucmd$ = "CEFHJKNPQRX"
   if seclevel > 6 then menucmd$ = menucmd$ + "U"
   menu$ = "C Continuous read\E Email\F Filter\H Help\K Kill\J Jump\N Next\P Previous\Q Quit\R Reread\U User edit *\X Exit subboard\"
   if readloc = readend_
      then call printmenu("Last Message")_
      else call printmenu("Message")
   selection = instr(menucmd$,cmd$)
   if cmd$ = chr$(13) then readnext
   on selection goto readcontinuous, reademail, readfilter, readhelp,_
      readjump, readkill, readnext, readprev, readquit, readmsg, readquit,_
      readedituser

readcontinuous:
   readcont = 1
   goto readnext

reademail:
   emailid = msgid
   emailtitle$ = "Re: " + msgtitle$
   gosub emailchkid
   goto readprompt

readedituser:
   searchlist$ = "E"
   editid = msgid
   gosub edituser
   goto readprompt

readfilter:
   if fnfiltered(boardlist(readboard)) = 0_
      then mid$(filter$,filterbyte,1) = chr$(asc(mid$(filter$,filterbyte,1))_
           + 2 ^ filterbit) :_
           call prt(chr$(13) + "Board is now filtered." + chr$(13)) :_
           cmd$ = "X" :_
           goto readquit
   goto readprompt

readhelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\MSGRD.HLP",0)
   goto readprompt

readjump:
   call prt(chr$(13) + "Messages from #" + str$(readstart) + " to #"_
      + str$(readend) + chr$(13))
   call inpt(chr$(13) + "Enter number to jump to: ",msg$,5)
   if fntruenum(msg$) = 0 then readprompt
   jumpvalue = val(msg$)
   if msg$ = "" or jumpvalue = 0 then readprompt
   if jumpvalue >= readstart and jumpvalue <= 255_
      then readloc = jumpvalue
   if readloc > readend and allnew = 0_
      then call prt(chr$(13) + "No more messages." + chr$(13)) : goto readquit_
      else if readloc > readend then readquit
   goto readmsg

readnext:
   incr readloc
   if readloc > readend_
      then if allnew = 0_
              then call prt(chr$(13) + "No more messages." + chr$(13)) :_
                   goto readquit_
              else readquit_
      else readmsg

readprev:
   decr readloc
   if readloc = 0_
      then call prt(chr$(13) + "Beginning of messages." + chr$(13)) :_
           incr readloc :_
           goto readprompt_
      else readmsg

readkill:
   if msgid <> userid and seclevel < 7_
      then call prt(chr$(13) + "That message isn't yours." + chr$(13)) :_
      goto readprompt
   if fnfileexist(filenm$) = 1 then kill filenm$
   if readend <> 255_
      then for readshift = readloc + 1 to 255 :_
              get #6, (256 * boardlist(readboard)) - 256 + readshift :_
              put #6, (256 * boardlist(readboard)) - 256 + readshift - 1 :_
           next readshift
   get #6, 256 * boardlist(readboard)
   value = cvi(msgnum$)
   decr value
   lset msgnum$ = mki$(value)
   put #6, 256 * boardlist(readboard)
   decr readend
   if readloc > readend_
      then readnext_
      else readmsg

readquit:
   allnewsub = readboard
   nest = nest - 1
   return
