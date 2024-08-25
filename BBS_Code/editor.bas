'Black Hole BBS editor
'By Brad Hufnagel
'-----------------------------------------------------------------------------

sub editor(filenm$,maxlines)
   shared abortable, aborted, baseaddress, carrier, comport, cmd$
   shared expert, menu$, menucmd$, edsave, lines, message$(), remote, uc
   shared termstatus, videowidth, nest, cursorx, cursory
   shared userid, realname$, handle$, title$
   incr nest

lineeditor:
   abortable = 0 : column = 0 : lines = 1
   for counter = 1 to 50 : message$(counter) = "" : next counter
   call flushin
   termstatus = termstatus or 128
   if expert_
      then call prt(chr$(13) + "Enter your message.  <ESC> or /EX exits." +_
              chr$(13) + chr$(13))_
      else call prt(chr$(13) + "Enter your message," + str$(maxlines) +_
              " lines maximum.  Hit <ESC>, or type /EX on a new line" +_
              " to end." + chr$(13) + chr$(13))

editloop:
   call carrierchk
   if not carrier and remote then carrierdrop
   call readchr(charloc$,charrem$)
   if charloc$ = chr$(0) and charrem$ = chr$(0) then editloop
   if charloc$ = chr$(0)_
      then char$ = charrem$_
      else char$ = charloc$
   if char$ = chr$(127) then char$ = chr$(8)
   charval = asc(char$)
   select case charval
      case = 8
         length = len(message$(lines))
         if length = 0_
            then if lines = 1_
                    then exit select_
                    else decr lines :_
                         call prt(chr$(0) + chr$(13) + message$(lines)) :_
                         column = len(message$(lines)) :_
                         exit select
         if length = 1_
            then call prt(chr$(0) + chr$(8) + chr$(32) + chr$(8)) :_
                 message$(lines) = "" :_
                 decr column :_
                 exit select
         if length > 1_
            then call prt(chr$(0) + chr$(8) + chr$(32) + chr$(8)) :_
                 message$(lines) = left$(message$(lines),length - 1) :_
                 decr column :_
                 exit select
      case = 9
         if ((videowidth - 1) - column) > 5_
            then call prt(chr$(0) + "     ") :_
                 message$(lines) = message$(lines) + string$(5,32) :_
                 column = column + 5 :_
                 exit select
      case = 13
         call prt(chr$(0) + chr$(13))
         incr lines
         column = 0
         message$(lines) = ""
         exit select
      case = 24
         length = len(message$(lines))
         if length = 0 then exit select
         for counter = 1 to length
            call prt(chr$(0) + chr$(8) + chr$(32) + chr$(8))
         next counter
         column = 0
         message$(lines) = ""
         exit select
      case = 27
         call prt(chr$(0) + chr$(13))
         if message$(lines) = ""_
            then decr lines
         goto editormenu
   end select
   if charval = 13 then editlinechk
   if charval < 31 or charval = 126 then editloop
   incr column
   call prt(chr$(0) + char$)
   message$(lines) = message$(lines) + char$
   if column < (videowidth - 1)_
      then editlinechk
   if instr(message$(lines),chr$(32)) = 0_
      then message$(lines + 1) = "" :_
           column = 0 :_
           goto editformatcr
   for editcount = column to 1 step - 1
      if (mid$(message$(lines),editcount,1) = chr$(32))_
         and (editcount <> column)_
         then exit for
   next editcount
   for editbcksp = editcount to column
      call prt(chr$(0) + chr$(8) + chr$(32) + chr$(8))
   next editbcksp
   message$(lines + 1) = right$(message$(lines),column - editcount)
   message$(lines) = left$(message$(lines),editcount)
   column = len(message$(lines + 1))
   editformatcr:
      call prt(chr$(0) + chr$(13))
      incr lines
      if lines < = maxlines_
         then call prt(chr$(0) + message$(lines)) :_
              goto editend
   editlinechk:
      if (lines = maxlines - 5) and (column = 0)_
         then call prt(chr$(0) + chr$(7) +_
                 "  -- Warning: only 5 lines left! -- " + chr$(13))
      if lines > maxlines then lines = maxlines : goto editormenu
      chk$ = ucase$(message$(lines))
      if chk$ = "/EX" or chk$ = "%Q" or chk$ = ".S" or chk$ = "/S"_
         then call prt(chr$(0) + chr$(13)) :_
              message$(lines) = "" :_
              decr lines :_
              if chk$ = "/S" then savemsg else editormenu
editend:
   goto editloop

editormenu:
   termstatus = termstatus and 127
   menucmd$ = "CDHILQRS"
   menu$ = "C Continue message\D Delete line\H Help\I Insert line\L List message\Q Quit\R Replace line\S Save message\"
   call printmenu("Editor")
   selection = instr(menucmd$,cmd$)
   if selection = 0_
      then editormenu_
      else on selection goto continuemsg, deleteline, editorhelp, insertline,_
         listmsg, quit, replaceline, savemsg

continuemsg:
   column = 0
   incr lines
   call prt(chr$(13) + "Continue entry:" + chr$(13) + chr$(13))
   goto editloop

deleteline:
   call inpt(chr$(13) + "Delete which line? ",lineno$,2)
   if lineno$ = chr$(13) then editormenu
   value = val(lineno$)
   if value = 0 or value > lines_
      then call prt("Invalid line." + chr$(13)) : goto editormenu
   uc = 1
   call inpt(chr$(13) + "Are you sure? (y/n) ",chk$,0)
   if chk$ <> "Y" then editormenu
   message$(value) = "" : decr lines
   if value <> lines + 1_
      then for counter = value to lines :_
              message$(counter) = message$(counter + 1) :_
           next counter
   call prt(chr$(13) + "Line #" + str$(value) + " deleted." + chr$(13))
   goto editormenu

editorhelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\EDITOR.HLP",0)
   goto editormenu

insertline:
   call inpt(chr$(13) + "Insert before which line? ",lineno$,2)
   if lineno$ = chr$(13) then editormenu
   value = val(lineno$)
   if value = 0 or value > lines_
      then call prt("Invalid line." + chr$(13)) : goto editormenu
   if lines = maxlines_
      then call prt(chr$(13) + "Sorry, but there's no more room in your " +_
           "message for new lines." + chr$(13)) : goto editormenu
   call inpt(chr$(13) + "Enter new line to insert, or [Cr] to abort:" +_
      chr$(13) + chr$(13),newline$,videowidth - 1)
   if newline$ = "" then call prt(chr$(13) + "Insert aborted." + chr$(13)) :_
      goto editormenu
   incr lines
   for counter = lines to value + 1 step - 1
   message$(counter) = message$(counter - 1)
   next counter
   message$(value) = newline$
   call prt(chr$(13) + "Line #" + str$(value) + " inserted." + chr$(13))
   goto editormenu

listmsg:
   uc = 1 : lineno = 0
   call inpt(chr$(13) + "With line numbers? ",lineno$,0)
   if lineno$ <> "Y" and lineno$ <> "N" then listmsg
   if lineno$ = "Y" then lineno = 1
   call prt(chr$(13))
   for msgloop = 1 to lines
      if lineno then call prt("#" + str$(msgloop) + ":" + chr$(13))
      call prt(message$(msgloop))
      if right$(message$(msgloop),1) <> chr$(13)_
         then call prt(chr$(13))
   next msgloop
   goto editormenu

quit:
   uc = 1 : call inpt(chr$(13) + "Quit?  Are you sure? ",quit$,0)
   if quit$ = "Y"_
      then cmd$ = "" : edsave = 0 : decr nest : exit sub_
      else editormenu

replaceline:
   call inpt(chr$(13) + "Replace which line? ",lineno$,2)
   if lineno$ = chr$(13) then editormenu
   value = val(lineno$)
   if value = 0 or value > lines_
      then call prt("Invalid line." + chr$(13)) : goto editormenu
   call prt(chr$(13) + "Line #" + str$(value) + " reads:" +_
      chr$(13) + chr$(13) +  message$(value) + chr$(13))
   call inpt(chr$(13) + "Enter new line, or [Cr] to abort:" +_
      chr$(13) + chr$(13),newline$,videowidth - 1)
   if newline$ = ""_
      then call prt("Line remains unchanged." + chr$(13)) : goto editormenu_
      else message$(value) = newline$ : goto editormenu

savemsg:
   while (lines > 1) and (message$(lines) = "")
      decr lines
   wend
   open filenm$ for output as #1
      if title$ = chr$(1) then outputmsg
      print #1, userid
      print #1, realname$
      print #1, handle$
      print #1, title$
      print #1, fnhistdate$;" ";left$(time$,5)
   outputmsg:
      for counter = 1 to lines
         print #1, message$(counter)
      next counter
   close #1
   decr nest
   exit sub

end sub
