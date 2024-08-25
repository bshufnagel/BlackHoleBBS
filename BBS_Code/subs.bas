'Black Hole BBS subroutines
'by Brad Hufnagel
'----------------------------------------------------------------------------

sub pop inline
   $inline "pop.com"
end sub

def fninitfossil(comport)
   reg 1, 4 * 256
   reg 2, 0
   reg 4, comport - 1
   call interrupt &h14
   if reg(1) = &h1954_
      then fninitfossil = 1_
      else fninitfossil = 0
end def

sub removefossil
   shared comport
   reg 1, 5 * 256
   reg 4, comport - 1
   call interrupt &h14
end sub

sub initport
   shared comport, maxbaud
   select case maxbaud
      case = 2400
         baudcode = 163
      case = 1200
         baudcode = 131
      case else
         baudcode = 67
   end select
   reg 1, baudcode
   reg 4, comport - 1
   call interrupt &h14
end sub

sub modemcmd(cmd$)
   shared comport, remote
   for x = 1 to len(cmd$)
      reg 1, 256 + asc(mid$(cmd$,x,1))
      reg 4, comport - 1
      call interrupt &h14
   next x
   reg 1, 256 + 13
   reg 4, comport - 1
   call interrupt &h14
   reg 1, 8 * 256
   reg 4, comport - 1
   call interrupt &h14
   do
     call response(modem$)
   loop until (modem$ = "OK") or (modem$ = "BAD")
end sub

sub response(modem$)
   shared comport, remote
   modem$ = "" : counter = 0 : sec1$ = time$
   do
      char = 0
      reg 1, 12 * 256
      reg 4, comport - 1
      call interrupt &h14
      if reg(1) = -1 then responseloop
      reg 1, 2 * 256
      reg 4, comport - 1
      call interrupt &h14
      char = reg(1) - 512
      if char > 31_
         then modem$ = modem$ + chr$(char)
   responseloop:
      sec2$ = time$
      if sec1$ <> sec2$_
         then incr counter : sec1$ = sec2$
   loop until (char = 13) or (counter = 3)
   if counter = 3 then modem$ = "BAD"
end sub

sub sendout
   shared comport
   empty = 0
   do
      reg 1, 3 * 256
      reg 4, comport - 1
      call interrupt &h14
      empty = (reg(1) \ 256) and 64
   loop until (empty = 64)
end sub

sub hangup
   shared baseaddress, comport, remote
   value = inp(baseaddress + 4)
   out (baseaddress + 4), value - 1
   delay 2
   out (baseaddress + 4), value
   call modemcmd("ATH0 S0=0")
end sub

sub setbaud(baudrate)
   shared baseaddress
   if baudrate = 300 then highbyte = 1 : lowbyte = 128
   if baudrate = 1200 then highbyte = 0 : lowbyte = 96
   if baudrate = 2400 then highbyte = 0 : lowbyte = 48
   oldvalue = inp(baseaddress + 3)
   out (baseaddress + 3), oldvalue or 128
   out baseaddress, lowbyte
   out (baseaddress + 1), highbyte
   out (baseaddress + 3), oldvalue
end sub

sub ringchk
   shared baseaddress, ring
   ring = inp(baseaddress + 6) and 64
   if ring = 64 then ring = 1
end sub

sub carrierchk
   shared baseaddress, carrier, comport, remote
   carrier = inp(baseaddress + 6) and 128
   if carrier = 128 then carrier = 1
end sub

sub flushin
   shared comport
   reg 1, 10 * 256
   reg 4, comport - 1
   call interrupt &h14
   do
      a$ = inkey$
   loop until a$ = ""
end sub

sub flushout
   shared comport
   reg 1, 9 * 256
   reg 4, comport - 1
   call interrupt &h14
end sub

sub mlrdchr inline
   $inline "bhinp.com"
end sub

sub readchr(charloc$,charrem$)
   shared comport, remote
   phrase$ = chr$(remote) + chr$(comport - 1)
   call mlrdchr(phrase$)
   charloc$ = left$(phrase$,1)
   charrem$ = mid$(phrase$,2,1)
end sub

sub addhistory(history$)
   shared bbspfx$
   filenm$ = bbspfx$ + "HISTORY.LOG"
   open filenm$ for append as #3
      print #3, history$;
   close #3
   filenm$ = bbspfx$ + "DAILY.LOG"
   open filenm$ for append as #3
      print #3, history$;
   close #3
end sub

sub adderror(syserr$)
   filenm$ = bbspfx$ + "ERROR.LOG"
   open filenm$ for append as #10
      print #10, fnhistdate$;" ";time$;" ";syserr$
   close #10
end sub

sub unpad(target$,dest$)
   dest$ = "" : position = len(target$)
   while mid$(target$,position,1) = " "
      position = position - 1
      if position = 0 then exit sub
   wend
   dest$ = left$(target$,position)
end sub

sub expanddatetime(target$,dest$)
   dest$ = ""
   for position = 1 to len(target$)
      char$ = str$(asc(mid$(target$,position,1)))
      char$ = right$(char$,len(char$)-1)
      if len(char$) = 1 then char$ = "0" + char$
      dest$ = dest$ + char$
   next position
end sub

sub datetimeformat(subject$)
   if len(subject$) = 4_
      then subject$ = left$(subject$,2) + ":" + right$(subject$,2)_
      else_
   if len(subject$) = 6_
      then subject$ = left$(subject$,2) + "/" + mid$(subject$,3,2)_
           + "/" + right$(subject$,2)_
      else_
   if len(subject$) = 10_
      then subject$ = left$(subject$,2) + "/" + mid$(subject$,3,2)_
           + "/" + mid$(subject$,5,2) + " " + mid$(subject$,7,2)_
           + ":" + right$(subject$,2)
end sub

sub compressdatetime(subject$)
   subject$ = chr$(val(left$(date$,2))) + chr$(val(mid$(date$,4,2)))_
      + chr$(val(right$(date$,2))) + chr$(val(left$(time$,2)))_
      + chr$(val(mid$(time$,4,2)))
end sub

def fnnormaldate$(indate$)
   dayname$ = "SunMonTueWedThuFriSat"
   month$ = "JanFebMarAprMayJunJulAugSepOctNovDec"
   year = val(mid$(indate$,7,2))
   month = val(left$(indate$,2))
   day = val(mid$(indate$,4,2))
   day$ = str$(day)
   day$ = right$(day$,len(day$) - 1)
   if indate$ = "" or month < 1 or day < 1_
      then fnnormaldate$ = "[No date]" : exit def
   ytemp = year : mtemp = month : dtemp = day
   mtemp = mtemp - 2 : if mtemp < 1 then mtemp = mtemp + 12 : ytemp = ytemp - 1
   x! = int(2.6 * mtemp - .19) + dtemp + ytemp + int(ytemp/4) - 34
   x! = x! - int(x!/7) * 7
   dayofweek$ = mid$(dayname$,((x! * 3) + 1),3)
   outdate$ = dayofweek$ + " " + mid$(month$,(month * 3) - 2,3) + " "_
       + day$ + ", " + "19" + mid$(indate$,7,2) + "  "
   if len(indate$) = 8_
      then fnnormaldate$ = outdate$ :_
           exit def
   hour = val(mid$(indate$,10,2))
   if hour = 0_
      then outtime$ = " 12"_
      else if hour > 12_
              then outtime$ = str$(hour - 12)_
              else outtime$ = str$(hour)
   outtime$ = right$(outtime$,len(outtime$) - 1) + ":"
   minute = val(mid$(indate$,13,2))
   minute$ = str$(minute)
   minute$ = right$(minute$,len(minute$) - 1)
   if len(minute$) = 1 then minute$ = "0" + minute$
   outtime$ = outtime$ + minute$
   if hour > 11_
      then outtime$ = outtime$ + "pm"_
      else outtime$ = outtime$ + "am"
   outdate$ = outdate$ + outtime$
   fnnormaldate$ = outdate$
end def

def fnhistdate$
   fnhistdate$ = left$(date$,2) + "/" + mid$(date$,4,2) + "/"_
                 + right$(date$,2)
end def

def fntruenum(number$)
   truenum = 1
   if val(number$) > 32767 or val(number$) < -32767_
      then fntruenum = 0 : exit def
   for counter = 1 to len(number$)
      charval = asc(mid$(number$,counter,1))
      if charval < 48 or charval > 57_
         then truenum = 0 : exit for
   next counter
   fntruenum = truenum
end def

def fntruefile(filename$)
   truefile = 1 : deccount = 0
   length = len(filename$)
   for position = 1 to length
      char$ = mid$(filename$,position,1)
      charval = asc(char$)
      if position > 9 and deccount = 0_
         then truefile = 0 : exit for
      if (charval = 33) or (charval = 44) or (charval = 45)_
         or (charval = 94) or (charval = 95) or (charval = 96)_
         or (charval = 123) or (charval = 124) or (charval = 126)_
         then fileloop
      if (charval >= 35 and charval <= 41) then fileloop
      if (charval >= 64 and charval <= 90) then fileloop
      if (charval >= 48 and charval <= 57) then fileloop
      if charval = 46 and (length - position) > 3_
         then truefile = 0 : exit for
      if charval = 46_
         then incr deccount :_
              if deccount > 1_
                 then truefile = 0 : exit for_
                 else fileloop
      truefile = 0 : exit for
   fileloop:
   next position
      if truefile = 0_
         then fntruefile = 0_
         else fntruefile = 1
end def

def fnfileexist(filename$)
   fileexist = 1
   on error goto fileexisterr
   name filename$ as filename$
   fileexisterr:
   if err = 53_
      then fileexist = 0 :_
           on error goto bbserror :_
           resume fileexistend
   on error goto bbserror
   resume fileexistend
   fileexistend:
   fnfileexist = fileexist
end def

def fnuldlratio(ultotal&,dltotal&)
   if dltotal& = 0_
      then fnuldlratio = ultotal& * 10_
      else fnuldlratio = (ultotal& \ dltotal&) * 100
end def

sub setboardlist
   shared boardlist(), boardname$(), boardread(), boardpost(),_
      totalsubboards, seclevel, type(), usertype
   if totalsubboards = 0 then boardlist(0) = 0 : exit sub
   position = 0 : workmap = 0
   for counter = 1 to 8
     if type(counter) = 1_
         then workmap = workmap + (2 ^ (counter - 1))
   next counter
   workmap = workmap and usertype
   for counter = 1 to 48
      boardmap = boardread(counter) or boardpost(counter)
      if ((boardmap and workmap) > 0 or seclevel > 8)_
         and (boardname$(counter) <> "")_
         then position = position + 1 : boardlist(position) = counter
   next counter
   boardlist(0) = position
end sub

sub setarealist
   shared arealist(), areaname$(), areaul(), areadl(),_
      filearea, totalareas, seclevel, type(), usertype
   if totalareas = 0 then arealist(0) = 0 : filearea = 0 : exit sub
   position = 0 : workmap = 0
   for counter = 1 to 8
      if type(counter) = 1_
         then workmap = workmap + (2 ^ (counter - 1))
   next counter
   workmap = workmap and usertype
   for counter = 1 to 48
      areamap = areaul(counter) or areadl(counter)
      if ((areamap and workmap) > 0 or seclevel > 8)_
         and (areaname$(counter) <> "")_
         then position = position + 1 : arealist(position) = counter
   next counter
   arealist(0) = position
   if arealist(0) = 0 then filearea = 0
end sub

sub timecheck(overtime)
   shared timeallowed, timeon, timeontoday
   overtime = 0
   timemins = timeon \ 60
   if (timeontoday + timemins) >= timeallowed_
      then overtime = 1
end sub

def fntimecnvt$(timesecs)
   timemins = 0 : timehours = 0 : value$ = ""
   if timesecs > 59_
      then timemins = timesecs \ 60 :_
           timesecs = timesecs - (timemins * 60)
   if timemins > 59_
      then timehours = timemins \ 60 :_
           timemins = timemins - (timehours * 60)
   if timehours > 0_
      then value$ = value$ + str$(timehours) + " hr"
   if timemins > 0_
      then value$ = value$ + str$(timemins) + " min"
   if timesecs > 0_
      then value$ = value$ + str$(timesecs) + " sec"
   fntimecnvt$ = value$
end def

def fntimediff(starttime$,stoptime$)
   if val(left$(stoptime$,2)) < val(left$(starttime$,2))_
      then mid$(stoptime$,1,2) = str$(val(left$(stoptime$,2)) + 24)
   startsec& = (val(left$(starttime$,2)) * 3600) +_
      (val(mid$(starttime$,4,2)) * 60) + val(right$(starttime$,2))
   stopsec& = (val(left$(stoptime$,2)) * 3600) +_
      (val(mid$(stoptime$,4,2)) * 60) + val(right$(starttime$,2))
   fntimediff = stopsec& - startsec&
end def

def fnfiltered(board)
   shared filter$, filterbyte, filterbit
   filterbyte = ((board - 1) \ 8) + 1
   filterbit = (board - 1) mod 8
   test$ = mid$(filter$,filterbyte,1)
   if (asc(test$) and 2 ^ filterbit) = 2 ^ filterbit_
      then fnfiltered = 1_
      else fnfiltered = 0
end def

sub brdcolor(attribute)
   shared abortable, aborted, baseaddress, bell, comport, carrier
   shared dump, fullscreen, nest, remote, termstatus, videowidth
   if (termstatus and 64) <> 64 then exit sub
   attrib$ = str$(attribute)
   attrib$ = right$(attrib$,len(attrib$) - 1)
   call prt(chr$(27) + "[" + attrib$ + "m")
end sub

sub shiftqueue(array(2),start)
   array(start,1) = 0
   array(start,2) = 0
   decr array(0,1)
   if (array(0,1) = 0) or (start > array(0,1)) then exit sub
   for count = start to array(0,1)
      array(count,1) = array(count + 1,1)
      array(count,2) = array(count + 1,2)
   next count
end sub

sub filelocchk(chkarea,position)
   shared arealist(), fileloc, totalfiles
   if fileloc <> 0 then exit sub
   if position <> totalfiles_
      then for counter = position + 1 to totalfiles :_
              get #8, (512 * arealist(chkarea)) - 512 + counter :_
              put #8, (512 * arealist(chkarea)) - 512 + counter - 1 :_
              lset filenum$ = mki$(0) :_
              put #8, (512 * arealist(chkarea)) - 512 + counter
           next counter
   decr totalfiles
   lset filenum$ = mki$(totalfiles)
   put #8, 512 * arealist(chkarea)
end sub

sub storecursor
   shared cursorx, cursory
   reg(1), 3840
   call interrupt &h10
   reg(1), 768
   call interrupt &h10
   cursory = reg(4) \ 256
   cursorx = reg(4) - cursory * 256
end sub

sub restorecursor
   shared cursorx, cursory
   reg(1), 3840
   call interrupt &h10
   reg(1), 512
   reg(4), (cursory * 256) + cursorx
   call interrupt &h10
end sub

sub blankscreen
   reg 1, 15 * 256
   call interrupt &h10
   reg 1, 6 * 256
   reg 2, 7 * 256
   reg 3, 0
   reg 4, 79 + (24 * 256)
   call interrupt &h10
   reg 1, 15 * 256
   call interrupt &h10
   reg 1, 2 * 256
   reg 4, 0
   call interrupt &h10
end sub

sub blankstatusline
   shared cursorx, cursory
   call storecursor
   reg 1, 15 * 256
   call interrupt &h10
   reg 1, 2 * 256
   reg 4, 23 * 256
   call interrupt &h10
   reg 1, 32 + (9 * 256)
   page = reg(2) \ 256
   reg 2, 112 + (page * 256)
   reg 3, 79
   call interrupt &h10

   reg 1, 2 * 256
   reg 4, 24 * 256
   call interrupt &h10
   reg 1, 32 + (9 * 256)
   page = reg(2) \ 256
   reg 2, 112 + (page * 256)
   reg 3, 79
   call interrupt &h10
   call restorecursor
end sub

sub printout(phrase$)
   reg 1, 15 * 256
   call interrupt &h10
   for counter = 1 to len(phrase$)
      char = asc(mid$(phrase$,counter,1))
      reg 1, char + (14 * 256)
      call interrupt &h10
      if char = 13_
         then reg 1, 10 + (14 * 256) :_
              call interrupt &h10
   next counter
end sub

sub printrow(phrase$,row)
   shared cursorx, cursory
   call storecursor
   reg 1, 15 * 256
   call interrupt &h10
   reg 1, 2 * 256
   reg 4, (row - 1) * 256
   call interrupt &h10
   call printout(phrase$)
   call restorecursor
end sub

sub statusdraw
   shared cursorx, cursory, handle$, realname$, userid, seclevel, phone$
   shared lastcalldate$
   call blankstatusline
   temp1$ = space$(4)
   temp2$ = space$(20)
   temp3$ = space$(20)
   rset temp1$ = str$(userid)
   lset temp2$ = handle$
   lset temp3$ = realname$
   phrase1$ = " #" + temp1$ + "  " + temp2$ + "  SL:" + str$(seclevel)
   if left$(lastcalldate$,1) <> chr$(0)_
      then phrase1$ = phrase1$ + "  Last on: " + fnnormaldate(lastcalldate$)
   phrase2$ = string$(8,32) + temp3$ + "  (" + left$(phone$,3) +_
      ") " + mid$(phone$,4,3) + "-" + right$(phone$,4)
   call printrow(phrase1$,24)
   call printrow(phrase2$,25)
end sub

sub mlprt inline
   $inline "bhprt.com"
end sub

sub prt(phrase$)
   shared abortable, aborted, baseaddress, bell, bye, comport, carrier
   shared dump, fullscreen, nest, remote, termstatus, videowidth
   nest = nest + 1
   if bye = 1 then prtsetup
   call carrierchk
   if not carrier and remote then carrierdrop
   prtsetup:
   aborted = 0
   status = (bell * 64) + (fullscreen * 32) + (abortable * 8) + (carrier * 4)_
            + (remote * 2) + (comport - 1)
   if left$(phrase$,1) = chr$(0)_
      then status = status + 128 :_
           phrase$ = right$(phrase$,len(phrase$) - 1)
   prtout$ = chr$(status) + chr$(termstatus) + chr$(videowidth - 1) + phrase$
   call mlprt(prtout$)
   status = asc(left$(prtout$,1))
   if abortable = 1 and ((status and 16) = 16) then aborted = 1
   if dump = 1 then print #9, phrase$
   call sendout
   nest = nest - 1
end sub

sub inpt(msg$,inpt$,lenofinput)
   shared baseaddress, carrier, comport, hidden, nest, remote, timeallowed
   shared timeon, timeontoday, timeout, uc
   nest = nest + 1
   call prt(msg$)
   length = 0 : inpt$ = "" : ring1 = 0 : ring2 = 0
   timestart = timeon
   timehalf = timeout \ 2 : timequarter = timehalf + (timehalf \ 2)
   getkybd:
      call carrierchk
      if not carrier and remote then carrierdrop
      call timecheck(overtime)
      if overtime then maxtime
      timechk = timeon - timestart
      if (timechk = timehalf) and (ring1 = 0)_
         then call prt(chr$(7)) : ring1 = 1
      if (timechk = timequarter) and (ring2 = 0)_
         then call prt(chr$(7)) : ring2 = 1
      if timechk > = timeout then timedout
      call readchr(charloc$,charrem$)
      if charloc$ = chr$(0) and charrem$ = chr$(0) then getkybd
      if charloc$ = chr$(0)_
         then char$ = charrem$_
         else char$ = charloc$
      if uc then char$ = ucase$(char$)
      if char$ = chr$(127) then char$ = chr$(8)
      value = asc(char$)
      if value < 32 and (value <> 13 and value <> 8 and value <> 24)_
         then getkybd
      if value = 8 and length = 0_
         then call prt(chr$(7)) : goto getkybd
      if value = 8 and length = 1_
         then call prt(chr$(0) + chr$(8) + chr$(32) + chr$(8)) :_
              length = 0 :_
              inpt$ = "" :_
              goto getkybd
      if value = 8 and length > 1_
         then call prt(chr$(0) + chr$(8) + chr$(32) + chr$(8)) :_
              length = length - 1 :_
              inpt$ = left$(inpt$, len(inpt$) - 1) :_
              goto getkybd
      if value = 13 and lenofinput <> 0_
         then call prt(chr$(13)) : hidden = 0 : uc = 0 :_
              nest = nest - 1 : exit sub
      if value = 13 and lenofinput = 0_
         then inpt$ = char$ : call prt(chr$(13)) : hidden = 0 :_
              uc = 0 : nest = nest - 1 : exit sub
      if value = 24 and length <> 0_
         then for countlen = 1 to length :_
                 call prt(chr$(0) + chr$(8) + chr$(32) + chr$(8)) :_
              next countlen :_
              inpt$ = "" :_
              length = 0 :_
              goto getkybd
      if (length = lenofinput) and (lenofinput <> 0)_
         then call prt(chr$(7)) : goto getkybd
      if hidden_
         then call prt("*")_
         else call prt(char$)
      inpt$ = inpt$ + char$
      length = length + 1
      if length = 1 and lenofinput = 0_
         then call prt(chr$(13)) : hidden = 0 : uc = 0 :_
              nest = nest - 1 : exit sub
      goto getkybd
end sub

sub printmenu(prompt$)
   shared abortable, aborted, baseaddress, bell, carrier, chat, cmd$
   shared comport, expert, menu$, menucmd$, nest, remote, timeallowed, timeon
   shared termstatus, timeontoday, timeout, uc, logon
   shared filearea, areaname$(), arealist()
   nest = nest + 1
   call brdcolor(33)
   timehalf = timeout \ 2 : timequarter = timehalf + (timehalf \ 2)
   ring1 = 0 : ring2 = 0
   goto prompt
   displaymenu:
      call prt(chr$(13) + "Menu - ")
      call brdcolor(37)
      call prt(prompt$ + chr$(13) + chr$(13))
      call brdcolor(33)
      abortable = 1 : text$ = ""
      for position = 1 to len(menu$)
         test$ = mid$(menu$,position,1)
         if test$ <> "\"_
            then text$ = text$ + test$_
            else if instr(menucmd$,left$(text$,1)) <> 0_
                    then text$ = "[" + left$(text$,1) + "]" +_
                         right$(text$,len(text$)-1) :_
                         call prt(text$ + chr$(13)) :_
                         text$ = ""_
                    else text$ = ""
         if aborted = 1 then abortable = 0 : exit for
      next position
   prompt:
      if chat_
         then remotestatus = remote :_
              remote = 0 :_
              call prt(chr$(7)) :_
              remote = remotestatus
      call flushin
      abortable = 0 : uc = 1
      call brdcolor(37)
      if filearea <> 0 and prompt$ = "File transfers"_
         then call prt(chr$(13) + "File area: " +_
             areaname$(arealist(filearea)) + ", #" +_
             str$(filearea) + chr$(13))_
         else call prt(chr$(13))
      call prt(prompt$)
      call prt(" [?=menu] ")
      call brdcolor(33)
      timestart = timeon
   getcmd:
      call carrierchk
      if not carrier and remote then carrierdrop
      call timecheck(overtime)
      if overtime then maxtime
      timechk = timeon - timestart
      if (timechk = timehalf) and (ring1 = 0)_
         then call prt(chr$(7)) : ring1 = 1
      if (timechk = timequarter) and (ring2 = 0)_
         then call prt(chr$(7)) : ring2 = 1
      if timechk > = timeout then timedout
      call readchr(charloc$,charrem$)
      if charloc$ = chr$(0) and charrem$ = chr$(0) then getcmd
      if charloc$ = chr$(0)_
         then cmd$ = charrem$_
         else cmd$ = charloc$
      cmd$ = ucase$(cmd$)
      if instr(menucmd$,cmd$) = 0 and cmd$ <> "?" and cmd$ <> chr$(13)_
         and cmd$ <> chr$(20)_
         then call prt(chr$(7)) : goto getcmd
      if cmd$ <> chr$(13) and cmd$ <> chr$(20)_
         then call prt(cmd$ + chr$(13))_
         else call prt(chr$(13))
      if cmd$ = "?" or_
         (cmd$ = chr$(13) and (prompt$ <> "Message"_
         and prompt$ <> "Last Message" and prompt$ <> "Email"_
         and prompt$ <> "Last Email"))_
         then displaymenu
      if cmd$ = chr$(20)_
         then call prt(chr$(13) + fnnormaldate$(left$(date$,6) +_
              right$(date$,2) + " " + left$(time$,5)) + chr$(13)) :_
              call prt("You've been on for"  +_
              fntimecnvt$(timeon) + "." + chr$(13)) :_
              if expert then prompt else displaymenu
      call brdcolor(33)
      nest = nest - 1
end sub

sub fileprt(filename$,msgnumber)
   shared abortable, aborted, baseaddress, bell, carrier, comport, fullscreen
   shared msgid, msgname$, msgtitle$, nest, remote, videowidth, userhandle$
   shared pause, readend, seclevel, cursorx, bye
   nest = nest + 1
   call flushin
   if (fnfileexist(filename$) = 0) and (msgnumber <> 0)_
      then call prt(chr$(13) + "File not found." + chr$(13))
   if fnfileexist(filename$) = 0 then readfileexit
   open filename$ for input as #1
   if eof(1) then readfileend
   if msgnumber = 0 then readfile
   input #1, msgid
   line input #1, msgname$
   line input #1, msghandle$
   line input #1, msgtitle$
   line input #1, msgdate$
   call brdcolor(33)
   abortable = 1
   if aborted then readfileend
   msg$ = str$(msgnumber) + "/" + right$(str$(readend),len(str$(readend)) - 1)
   call prt(chr$(13) + right$(msg$,len(msg$) - 1)  + ": " + msgtitle$ +_
      chr$(13))
   if aborted then readfileend
   tempid$ = right$(str$(msgid),len(str$(msgid)) - 1)
   call prt("From: " + msghandle$ + " (#" + tempid$ + ")" + chr$(13))
   if aborted then readfileend
   call prt("Date: " + fnnormaldate$(msgdate$) + chr$(13))
   if aborted then readfileend
   if seclevel > 6_
      then call prt("Filename: " + filename$ + chr$(13))
   if aborted then readfileend

readfile:
   readstart = 1 : readcr = 0 : prev$ = ""
   call brdcolor(32)
   call prt(chr$(13))
   do while not eof(1) and aborted = 0
      line input #1, text$
      if text$ = "" and readcr = 1_
         then text$ = chr$(13) : goto readfileprint
      if text$ = "" and readcr = 0_
         then text$ = chr$(13) + chr$(13) : readcr = 1 : goto readfileprint
      if readstart = 1 then readstart = 0 : goto readfileprint
      if left$(text$,1) = " " and readcr = 0 then text$ = chr$(13) + text$	
      if text$ <> "" then readcr = 0
      readfileprint:
      call storecursor
      if (prev$ > chr$(32)) and (left$(text$,1) > chr$(32))_
         and ((cursorx + instr(text$," ")) < videowidth)_
         then text$ = " " + text$
      call prt(text$)
      prev$ = right$(text$,1)
   loop
   if aborted = 0 then call prt(chr$(13))
readfileend:
   close #1
readfileexit:
   abortable = 0
   call brdcolor(33)
   nest = nest - 1
   call flushin
end sub
