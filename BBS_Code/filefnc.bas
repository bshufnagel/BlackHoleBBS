'Black Hole BBS file transfer functions
'By Brad Hufnagel
'----------------------------------------------------------------------------

filefunctions:
   menucmd$ = "ACDEFHLQUS"
   if seclevel >= 3 then menucmd$ = menucmd$ + "MR"
   if seclevel >= 9 then menucmd$ = menucmd$ + "KXZ*"
   menu$ = "A All new files\C Change file area\D Download\E Expanded file area listing\F Find a file\H Help\K Kill file *\L List file areas\M Make file area\N New files\Q Quit\R Reconfigure file area\S Scan file area\" +_
      "U Upload\X Sort file area *\Z Zap file area\* Reset file area *\"
   call printmenu("File transfers")
   selection = instr(menucmd$,cmd$)
   call addhistory(lcase$(cmd$))
   on selection goto allnewfiles, changearea, downloadfile, expandedarea,_
      findfile, filehelp, listareas, mainmenu, uploadfile, scanfiles,_
      makearea, reconfigarea, killfile, sortarea, zaparea, resetfiles

filehelp:
   abortable = 1
   call fileprt(bbspfx$ + "TEXT\FILE.HLP",0)
   goto filefunctions

allnewfiles:
   if arealist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any file areas." +_
              chr$(13)) :_
           goto filefunctions
   call prt(chr$(13) + "Searching all file areas..." + chr$(13))
   newfile = 0 : abortable = 1
   for counter = 1 to arealist(0)
      get #8, 512 * arealist(counter)
      totalfiles = cvi(filenum$)
      if totalfiles = 0 then allnewcounter
      for counter2 = 1 to totalfiles
         get #8, (512 * arealist(counter)) - 512 + counter2
         fileloc = cvi(filenum$)
         get #5, fileloc
         fdptr = cvi(fdptr$)
         if lastmsgptr > fdptr then allnewcounter2
         if newfile = 0_
            then newfile = 1 :_
                 call prt(chr$(13) + areaname$(arealist(counter)) + chr$(13)_
                    + string$(len(areaname$(arealist(counter))),"-") +_
                    chr$(13) + chr$(13))
         if aborted then exit for
         call unpad(fdname$,temp$)
         fsname$ = space$(12)
         lset fsname$ = temp$
         fssize$ = space$(5)
         rset fssize$ = str$(cvi(fdsize$))
         call unpad(fddesc$,fsdesc$)
         call prt(fsname$ + " :" + fssize$ + ": " + fsdesc$ + chr$(13))
         if aborted then exit for
      allnewcounter2:
      next counter2
      newfile = 0
      if aborted then exit for
   allnewcounter:
   next counter
   goto filefunctions

changearea:
   if arealist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any file areas." +_
              chr$(13)) :_
           goto filefunctions
   call inpt(chr$(13) + "Change to which file area [1 -" +_
      str$(arealist(0)) + "] ",area$,2)
   area = val(area$)
   if area$ = "" then filefunctions
   if area < 1 or area > arealist(0)_
      then call prt(chr$(13) + "Invalid area." + chr$(13)) :_
           goto filefunctions
   filearea = area
   call prt(chr$(13) + areaname$(arealist(filearea)) + " selected." +_
      chr$(13))
   goto filefunctions

reconfigarea:
   if (systype and usertype) = 0_
      then call prt(chr$(13) + "You don't have the proper user types to " +_
              "reconfigure a file area." + chr$(13)) :_
           goto filefunctions
   reconfignum = filearea
   if reconfignum = 0_
      then call prt(chr$(13) + "No file area to reconfigure." + chr$(13)) :_
           goto filefunctions
   if (areaowner(arealist(reconfignum)) <> userid) and (seclevel < 7)_
      then call prt(chr$(13) + "This file area isn't yours!" + chr$(13)) :_
           goto filefunctions
   uc = 0
   call inpt(chr$(13) + "Enter the new name of your file area." +_
      chr$(13) + ": ",tempname$,50)
   if tempname$ = "" then filefunctions
   reconfigfstats:
   ulmap = 0 : dlmap = 0
   for counter = 1 to 8
      if type(counter) = 0 then reconfigfnext
      if (usertype and (2 ^ (counter - 1))) = 0 then reconfigfnext
      reconfigfulchk:
      call prt(chr$(13))
      uc = 1
      call inpt("Can a " + typename$(counter) + " upload to your file " +_
         "area? (y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ = "Y"_
         then ulmap = ulmap + (2 ^ (counter - 1))_
         else if value$ <> "N" then reconfigfulchk
      reconfigfdlchk:
      uc = 1
      call inpt("Can a " + typename$(counter) + " download from your file " +_
         "area? (y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ = "Y"_
         then dlmap = dlmap + (2 ^ (counter - 1))_
         else if value$ <> "N" then reconfigfdlchk
   reconfigfnext:
   next counter
   if value$ = "Q" then filefunctions
   if ulmap = 0 and dlmap = 0_
      then call prt(chr$(13) + "Someone has to be able to do SOMETHING " +_
              "on your file area!" + chr$(13)) : goto reconfigfstats
   tempid = areaowner(arealist(reconfignum))
   if (seclevel > 6) and (tempid <> userid)_
      then uc = 1 :_
           call inpt(chr$(13) + "This file area was not originally yours.  " +_
              "Do you want to be the owner? (y/n) ",value$,0) :_
           if value$ = "Y" then tempid = userid
   areaname$(arealist(reconfignum)) = tempname$
   areaowner(arealist(reconfignum)) = tempid
   areaul(arealist(reconfignum)) = ulmap
   areadl(arealist(reconfignum)) = dlmap
   call prt(chr$(13) + "Your file area is #" + str$(reconfignum) + "." +_
      chr$(13))
   goto filefunctions

expandedarea:
   if arealist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any file areas." +_
              chr$(13)) :_
           goto filefunctions
   call inpt(chr$(13) + "Start listing at [1 -" + str$(arealist(0)) +_
      "] ",start$,2)
   start = val(start$)
   if start$ = "" then start = 1
   if start < 1 or start > arealist(0) then filefunctions
   call prt(chr$(13))
   abortable = 1
   for counter = start to arealist(0)
      get #2, areaowner(arealist(counter))
      call unpad(userhandle$,maker$)
      call prt(" ")
      if counter < 10 then call prt(" ")
      call prt(str$(counter) + ". " + areaname$(arealist(counter)) + " [" +_
         maker$ + "] " + chr$(13))
      if aborted then exit for
      call prt("            Upload: ")
      if aborted then exit for
      start = 0
      for counter2 = 1 to 8
         if type(counter2) = 0 then expandulnext
         if ((areaul(arealist(counter)) and 2 ^ (counter2 - 1))_
            = (2 ^ (counter2 - 1))) and ((usertype and 2 ^ (counter2 - 1))_
            = (2 ^ (counter2 - 1)))_
            then if start = 0_
                    then start = 1 : call prt(typename$(counter2))_
                    else call prt(", " + typename$(counter2))
         if aborted then exit for
      expandulnext:
      next counter2
      if aborted then exit for
      call prt(chr$(13) + "          Download: ")
      if aborted then exit for
      start = 0
      for counter2 = 1 to 8
         if type(counter2) = 0 then expanddlnext
         if ((areadl(arealist(counter)) and 2 ^ (counter2 - 1))_
            = (2 ^ (counter2 - 1))) and ((usertype and 2 ^ (counter2 - 1))_
            = (2 ^ (counter2 - 1)))_
            then if start = 0_
                    then start = 1 : call prt(typename$(counter2))_
                    else call prt(", " + typename$(counter2))
         if aborted then exit for
      expanddlnext:
      next counter2
      call prt(chr$(13) + chr$(13))
      if aborted then exit for
   next counter
   abortable = 0
   goto filefunctions

listareas:
   if arealist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any file areas." +_
              chr$(13)) :_
           goto filefunctions
   call inpt(chr$(13) + "Start listing at [1 -" + str$(arealist(0)) +_
      "] ",start$,2)
   start = val(start$)
   if start$ = "" then start = 1
   if start < 1 or start > arealist(0) then filefunctions
   call prt(chr$(13))
   abortable = 1
   for counter = start to arealist(0)
      if counter < 10 then call prt(" ")
      if aborted then exit for
      call prt(str$(counter) + ". " + areaname$(arealist(counter)))
      if aborted then exit for
      call prt(chr$(13))
      if aborted then exit for
   next counter
   abortable = 0
   goto filefunctions

makearea:
   multiple = 0
   for counter = 1 to totalareas
      if areaowner(counter) = userid_
         then multiple = 1
   next counter
   if seclevel = 3 and multiple_
      then call prt(chr$(13) + "You already have a file area." +_
              chr$(13)) :_
           goto filefunctions
   if (systype and usertype) = 0_
      then call prt(chr$(13) + "You don't have the proper user types to " +_
              "make a file area." + chr$(13)) :_
           goto filefunctions
   if totalareas = 48 then_
      call prt(chr$(13) + "Sorry, but the maximum file area limit has " +_
         "been reached." + chr$(13) + chr$(13)) :_
      goto filefunctions
   uc = 0
   call inpt(chr$(13) + "Enter the name of your new file area." +_
      chr$(13) + ": ",fileareaname$,50)
   if fileareaname$ = "" then filefunctions
   makefstats:
   ulmap = 0 : dlmap = 0
   for counter = 1 to 8
      if type(counter) = 0 then makefnext
      if (usertype and (2 ^ (counter - 1))) = 0 then makefnext
      makefulchk:
      call prt(chr$(13))
      uc = 1
      call inpt("Can a " + typename$(counter) + " upload to your file " +_
         "area? (y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ = "Y"_
         then ulmap = ulmap + (2 ^ (counter - 1))_
         else if value$ <> "N" then makefulchk
      makefdlchk:
      uc = 1
      call inpt("Can a " + typename$(counter) + " download from your " +_
         "file area? (y/n/q) ",value$,0)
      if value$ = "Q" then exit for
      if value$ = "Y"_
         then dlmap = dlmap + (2 ^ (counter - 1))_
         else if value$ <> "N" then makefdlchk
   makefnext:
   next counter
   if value$ = "Q" then filefunctions
   if ulmap = 0 and dlmap = 0_
      then call prt(chr$(13) + "Someone has to be able to do SOMETHING " +_
              "on your file area!" + chr$(13)) : goto makefstats
   for counter = 1 to totalareas
      if areaname$(counter) = "" then exit for
   next counter
   areaname$(counter) = fileareaname$
   areaowner(counter) = userid
   areaul(counter) = ulmap
   areadl(counter) = dlmap
   areaptr(counter) = sysmsgptr
   totalareas = totalareas + 1
   if filearea = 0 then filearea = 1
   call setarealist
   for areapos = 1 to totalareas
      if arealist(areapos) = counter then exit for
   next areapos
   call prt(chr$(13) + "Your file area is #" + str$(areapos) + "." + chr$(13))
   goto filefunctions

findfile:
   if arealist(0) = 0_
      then call prt(chr$(13) + "You don't have access to any file areas." +_
              chr$(13)) :_
           goto filefunctions
   uc = 1
   call inpt(chr$(13) + "Enter filename: ",filenm$,12)
   if filenm$ = "" then filefunctions
   if fntruefile(filenm$) = 0_
      then call prt(chr$(13) + "Invalid filename format." + chr$(13)) :_
           goto filefunctions
   call prt(chr$(13) + "Searching all file areas..." + chr$(13) + chr$(13))
   for counter = 1 to arealist(0)
      get #8, 512 * arealist(counter)
      totalfiles = cvi(filenum$)
      if totalfiles = 0 then findfileloop
      for counter2 = 1 to totalfiles
         get #8, (512 * arealist(counter)) - 512 + counter2
         fileloc = cvi(filenum$)
         get #5, fileloc
         call unpad(fdname$,ffname$)
         if ffname$ = filenm$_
            then call prt(filenm$ + " is located in " +_
                    areaname$(file(counter,1)) + chr$(13))
      next counter2
   findfileloop:
   next counter
   goto filefunctions

sortarea:
   sortarea = filearea
   if sortarea = 0_
      then call prt(chr$(13) + "No file area to sort." + chr$(13)) :_
           goto filefunctions
   call prt(chr$(13) + "Sorting...")
   get #8, 512 * arealist(sortarea)
   totalfiles = cvi(filenum$)
   if totalfiles = 0_
      then call prt("no files are present in this file area." + chr$(13)) :_
           goto filefunctions
   for counter1 = 1 to totalfiles - 1
      for counter2 = counter1 + 1 to totalfiles
         get #8, (512 * arealist(sortarea)) - 512 + counter1
         get #5, cvi(filenum$)
         call unpad(fdname$,sort1name$)
         get #8, (512 * arealist(sortarea)) - 512 + counter2
         get #5, cvi(filenum$)
         call unpad(fdname$,sort2name$)
         if sort1name$ > sort2name$_
            then get #8, (512 * arealist(sortarea)) - 512 + counter1 :_
                 temp$ = filenum$ :_
                 get #8, (512 * arealist(sortarea)) - 512 + counter2 :_
                 put #8, (512 * arealist(sortarea)) - 512 + counter1 :_
                 lset filenum$ = temp$ :_
                 put #8, (512 * arealist(sortarea)) - 512 + counter2
      next counter2
   next counter1
   call prt("done!" + chr$(13))
   goto filefunctions

killfile:
   killarea = filearea
   if killarea = 0_
      then call prt(chr$(13) + "No file area to kill a file." + chr$(13)) :_
           goto filefunctions
   uc = 1
   call inpt(chr$(13) + "Enter filename: ",filenm$,12)
   if filenm$ = "" then filefunctions
   if fntruefile(filenm$) = 0_
      then call prt(chr$(13) + "Invalid filename format." + chr$(13)) :_
           goto filefunctions
   uc = 1 : call inpt(chr$(13) + "Kill actual file too? (y/n) ",value$,0)
   call prt(chr$(13) + "Searching...")
   get #8, 512 * arealist(killarea)
   totalfiles = cvi(filenum$)
   if totalfiles = 0_
      then call prt(chr$(13) + "No files are present in this file area." +_
              chr$(13)) :_
           goto filefunctions
   found = 0
   for counter = 1 to totalfiles
      get #8, (512 * arealist(killarea)) - 512 + counter
      get #5, cvi(filenum$)
      call unpad(fdname$,fkname$)
      if fkname$ = filenm$ then found = 1 : exit for
   next counter
   if found = 0_
      then call prt(filenm$ + " not found." + chr$(13)) :_
           goto filefunctions
   lset fdname$ = chr$(1) + right$(fkname$,len(fkname$) - 1)
   put #5, cvi(filenum$)
   position = counter
   if position <> totalfiles_
      then for counter = position + 1 to totalfiles :_
              get #8, (512 * arealist(killarea)) - 512 + counter :_
              put #8, (512 * arealist(killarea)) - 512 + counter - 1 :_
              lset filenum$ = mki$(0) :_
              put #8, (512 * arealist(killarea)) - 512 + counter
           next counter
   decr totalfiles
   lset filenum$ = mki$(totalfiles)
   put #8, 512 * arealist(killarea)
   filename$ = bbspfx$ + "FILES\" + filenm$
   if fnfileexist(filename$) = 1 and value$ = "Y" then kill filename$
   call prt(filenm$ + " deleted." + chr$(13))
   goto filefunctions

scanfiles:
   scanarea = filearea
   if scanarea = 0_
      then call prt(chr$(13) + "No file area to scan." + chr$(13)) :_
           goto filefunctions
   get #8, 512 * arealist(scanarea)
   totalfiles = cvi(filenum$)
   if totalfiles = 0_
      then call prt(chr$(13) + "No files are present in this file area." +_
              chr$(13)) :_
           goto filefunctions
   call prt(chr$(13) + areaname$(arealist(scanarea)) + chr$(13))
   call prt(string$(len(areaname$(arealist(scanarea))),"-") + chr$(13) +_
      chr$(13))
   call prt("Filename       Blks  Description" + chr$(13) + chr$(13))
   abortable = 1
   for counter = 1 to totalfiles
      get #8, (512 * arealist(scanarea)) - 512 + counter
      fileloc = cvi(filenum$)
      get #5, fileloc
      call unpad(fdname$,temp$)
      fsname$ = space$(12)
      lset fsname$ = temp$
      fssize$ = space$(5)
      rset fssize$ = str$(cvi(fdsize$))
      call unpad(fddesc$,fsdesc$)
      call prt(fsname$ + " :" + fssize$ + ": " + fsdesc$ + chr$(13))
      if aborted then exit for
   scanloop:
   next counter
   goto filefunctions

downloadfile:
   dlarea = filearea
   if dlarea = 0_
      then call prt(chr$(13) + "No file area to download from." +_
              chr$(13)) :_
           goto filefunctions
   goodmap = 0
   value = (systype and areadl(arealist(dlarea))) and usertype
   if seclevel > 8 then goodmap = 1
   for counter = 1 to 8
      if value and (2 ^ (counter - 1)) = (2 ^ (counter - 1))_
         and type(counter) = 1_
         then goodmap = 1
   next counter
   if goodmap = 0_
      then call prt(chr$(13) + "You don't have download access to that " +_
              "file area." + chr$(13)) :_
           goto filefunctions
   get #8, 512 * arealist(dlarea)
   totalfiles = cvi(filenum$)
   if totalfiles = 0_
      then call prt(chr$(13) + "No files are available for downloading " +_
              "on this file area." + chr$(13)) :_
           goto filefunctions
   if (uldlratio <> 0) and (fnuldlratio(uploadtotal&,downloadtotal&) <_
      uldlratio) and (seclevel < 9)_
      then call prt(chr$(13) + "Sorry, but you've downloaded too many " +_
         "files already.  Upload something to balance things out." +_
         chr$(13)) : goto filefunctions
   uc = 1
   call inpt(chr$(13) + "Enter filename: ",filenm$,12)
   if filenm$ = "" then filefunctions
   if fntruefile(filenm$) = 0_
      then call prt(chr$(13) + "Invalid filename format." + chr$(13)) :_
           goto filefunctions
   dlfound = 0
   for counter = 1 to totalfiles
      get #8, (512 * arealist(dlarea)) - 512 + counter
      fileloc = cvi(filenum$)
      get #5, fileloc
      call unpad(fdname$,fdlname$)
      if fdlname$ = filenm$ then dlfound = 1 : exit for
   dlfindloop:
   next counter
   if dlfound = 0_
      then call prt(chr$(13) + "Can't find " + filenm$ + " in the " +_
              areaname$(arealist(dlarea)) + " file area." + chr$(13)) :_
           goto filefunctions
   fdlsize$ = str$(cvi(fdsize$))
   if remote = 1_
      then fdlsec& = ((val(fdlsize$) * 128) / (val(baud$) / 8))_
      else fdlsec& = ((val(fdlsize$) * 128) / (1200 / 8))
   fdltime$ = fntimecnvt$(fdlsec&)
   fdltotal = cvi(fdtotal$)
   call unpad(fddate$,fdldate$)
   call unpad(fdhandle$,fdlhandle$)
   call unpad(fddesc$,fdldesc$)
   call prt(chr$(13))
   call prt("Filename    : " + fdlname$ + chr$(13))
   call prt("Description : " + fdldesc$ + chr$(13))
   call prt("# blocks    :" + fdlsize$ + chr$(13))
   call prt("Xfer time   :" + fdltime$ + chr$(13))
   call prt("Times D/L'd :" + str$(fdltotal) + chr$(13))
   call prt("Uploaded by : " + fdlhandle$ + chr$(13))
   call prt("Date        : " + fdldate$ + chr$(13))
   filename$ = bbspfx$ + "FILES\" + filenm$
   if remote = 0_
      then call prt(chr$(13) + "You can't download when you're logged on " +_
              "locally." + chr$(13)) :_
           goto filefunctions
   if (timeontoday + ((timeon + fdlsec&) / 60)) >= timeallowed_
      then call prt(chr$(13) + "You don't have enough time left to " +_
              "download that." + chr$(13)) :_
           goto filefunctions
   if fnfileexist(filename$) = 0_
      then call prt(chr$(13) + "File is currently off-line.  Ask the sysop" +_
              "about its availability." + chr$(13)) :_
           goto filefunctions
   menucmd$ = "1234Q"
   menu$ = "1 XMODEM\2 XMODEM/CRC\3 YMODEM\4 ZMODEM\Q Quit\"
   call printmenu("Protocol")
   selection = instr(menucmd$,cmd$)
   select case selection
      case = 1
         protocol$ = " sx " : tx$ = "XM-S" : txname$ = "XMODEM"
      case = 2
         protocol$ = " sx " : tx$ = "XC-S" : txname$ = "XMODEM/CRC"
      case = 3
         protocol$ = " sb " : tx$ = "YM-S" : txname$ = "YMODEM"
      case = 4
         protocol$ = " sz " : tx$ = "ZM-S" : txname$ = "ZMODEM"
      case = 5
         call prt(chr$(13) + "Transfer aborted." + chr$(13))
         goto filefunctions
   end select
   call prt(chr$(13) + txname$ + " protocol selected." + chr$(13))
   call prt(chr$(13) + "Ready to send " + filenm$ + chr$(13))
   call prt("Hit Control-X several times to abort transmission." +_
      chr$(13) + chr$(13))
   starttime$ = left$(time$,8)
   download$ = "dsz port" + str$(comport) + protocol$ + filename$
   call storecursor
   locate 24,1 : print string$(79,32);
   locate 25,1 : print string$(79,32);
   shell download$
   call restorecursor
   call statusdraw
   call prt(chr$(13) + "Transmission complete." + chr$(13))
   stoptime$ = left$(time$,8)
   if clockstop = 0 then timeon = timeon + fntimediff(starttime$,stoptime$)
   open filename$ as #4
   downloadtotal& = downloadtotal& + (lof(4) / 1024)
   close #4
   incr fdltotal
   lset fdtotal$ = mki$(fdltotal)
   put #5, fileloc
   call addhistory("[" + tx$ + " " + filenm$ + "]")
   goto filefunctions

uploadfile:
   ularea = filearea
   if ularea = 0_
      then call prt(chr$(13) + "No file area to upload to." + chr$(13)) :_
           goto filefunctions
   goodmap = 0
   value = (systype and areaul(arealist(ularea))) and usertype
   if seclevel > 8 then goodmap = 1
   for counter = 1 to 8
      if value and (2 ^ (counter - 1)) = (2 ^ (counter - 1))_
         and type(counter) = 1_
         then goodmap = 1
   next counter
   if goodmap = 0_
      then call prt(chr$(13) + "You don't have upload access to this file " +_
              "area." + chr$(13)) :_
           goto filefunctions
   get #8, 512 * arealist(ularea)
   totalfiles = cvi(filenum$)
   if totalfiles = 511_
      then call prt(chr$(13) + "Sorry, but this file area has reached its " +_
              "maximum file capacity." + chr$(13)) :_
           goto filefunctions
   uc = 1
   call inpt(chr$(13) + "Enter filename: ",filenm$,12)
   if filenm$ = "" then filefunctions
   if fntruefile(filenm$) = 0_
      then call prt(chr$(13) + "Invalid filename format." + chr$(13)) :_
           goto filefunctions
   uc = 1
   call inpt(chr$(13) + "Upload " + filenm$ + "? (y/n) ",value$,0)
   if value$ <> "Y" then filefunctions
   filename$ = bbspfx$ + "FILES\" + filenm$
   call inpt(chr$(13) + "Please enter a one-line description for " + filenm$ +_
      "." + chr$(13) + ": ",filedesc$,55)
   if filedesc$ = "" then filefunctions
   if remote = 0_
      then ullocal_
      else ulremote
ullocal:
   if fnfileexist(filename$) = 0_
      then call prt(chr$(13) + "Error - can't find " + filenm$ + "!" +_
              chr$(13)) :_
           call prt(chr$(13) + "Make sure that " + filenm$ +_
              " is present in the subdirectory FILES\ before you " +_
              "start your upload." + chr$(13)) :_
           goto filefunctions
   call prt(chr$(13) + "Local upload." + chr$(13))
   tx$ = "LCL-R"
   goto ulupdate
ulremote:
   if fnfileexist(filename$) = 1_
      then call prt(chr$(13) + "That file already exists!" + chr$(13) +_
              "Please use another name if this is a revised version of " +_
              "a file." + chr$(13)) :_
           goto filefunctions
   menucmd$ = "1234Q"
   menu$ = "1 XMODEM\2 XMODEM/CRC\3 YMODEM\4 ZMODEM\Q Quit\"
   call printmenu("Protocol")
   selection = instr(menucmd$,cmd$)
   select case selection
      case = 1
         protocol$ = " rx " : tx$ = "XM-R" : txname$ = "XMODEM"
      case = 2
         protocol$ = " rc " : tx$ = "XC-R" : txname$ = "XMODEM/CRC"
      case = 3
         protocol$ = " rb " : tx$ = "YM-R" : txname$ = "YMODEM"
      case = 4
         protocol$ = " rz " : tx$ = "ZM-R" : txname$ = "ZMODEM"
      case = 5
         call prt(chr$(13) + "Transfer aborted." + chr$(13))
         goto filefunctions
   end select
   call prt(chr$(13) + txname$ + " protocol selected." + chr$(13))
   call prt(chr$(13) + "Ready to receive " + filenm$ + chr$(13))
   call prt("Hit Control-X several times to abort transmission." + chr$(13))
   upload$ = "dsz port" + str$(comport) + protocol$ + filename$
   call storecursor
   locate 24,1 : print string$(79,32);
   locate 25,1 : print string$(79,32);
   shell upload$
   call restorecursor
   call statusdraw
ulupdate:
   call prt(chr$(13) + "Transmission complete." + chr$(13))
   incr totalfiles
   lset filenum$ = mki$(totalfiles)
   put #8, 512 * arealist(ularea)
   position = 0
   do
      incr position
      get #5, position
   loop until (eof(5) or (left$(fdname$,1) = chr$(1)))
   lset filenum$ = mki$(position)
   put #8, (512 * arealist(ularea)) - 512 + totalfiles
   open filename$ as #4
   fdsize = (lof(4) / 128)
   uploadtotal& = uploadtotal& + (lof(4) / 1024)
   close #4
   currentdate$ = left$(date$,6) + right$(date$,2) + left$(time$,5)
   lset fdname$ = filenm$
   lset fdsize$ = mki$(fdsize)
   lset fddate$ = fnnormaldate$(currentdate$)
   lset fdhandle$ = handle$
   lset fddesc$ = filedesc$
   lset fdtotal$ = mki$(0)
   lset fdptr$ = mki$(sysmsgptr)
   put #5, position
   call addhistory("[" + tx$ + " " + filenm$ + "]")
   call prt(chr$(13) + filenm$ + " has been uploaded." + chr$(13))
   goto filefunctions

zaparea:
   deletenum = filearea
   if filearea = 0_
      then call prt(chr$(13) + "No file area to delete." + chr$(13)) :_
           goto filefunctions
   if (areaowner(arealist(deletenum)) <> userid) and (seclevel < 9)_
      then call prt(chr$(13) + "That file area isn't yours!" + chr$(13)) :_
           goto filefunctions
   call prt(chr$(13) + "Zap " + areaname$(arealist(deletenum)) + "." +_
      chr$(13))
   uc = 1 : call inpt(chr$(13) + "Are you sure? (y/n) ",value$,0)
   if value$ <> "Y" then filefunctions
   call prt(chr$(13) + "Deleting " + areaname$(arealist(deletenum)) +_
      "." + chr$(13))
   get #8, 512 * arealist(deletenum)
   totalfiles = cvi(filenum$)
   if totalfiles = 0 then zapareawrite
   for counter = 1 to totalfiles
      get #8, (512 * arealist(deletenum)) - 512 + counter
      fileloc = cvi(filenum$)
      lset filenum$ = mki$(0)
      put #8, (512 * arealist(deletenum)) - 512 + counter
      get #5, fileloc
      call unpad(fdname$,filenm$)
      lset fdname$ = chr$(1) + right$(filenm$,len(filenm$) - 1)
      put #5, fileloc
      filename$ = bbspfx$ + "FILES\" + filenm$
      if fnfileexist(filename$) = 1 then kill filename$
   next counter
   zapareawrite:
   lset filenum$ = mki$(0)
   put #8, 512 * arealist(deletenum)
   areaname$(arealist(deletenum)) = ""
   areaowner(arealist(deletenum)) = 0
   arealist(deletenum) = 0
   totalareas = totalareas - 1
   filearea = 1
   call setarealist
   call prt(chr$(13) + "Deletion completed." + chr$(13))
   goto filefunctions

resetfiles:
   resetarea = filearea
   if resetarea = 0_
      then call prt(chr$(13) + "No file area to scan." + chr$(13)) :_
           goto filefunctions
   get #8, 512 * arealist(resetarea)
   total = cvi(filenum$)
   call prt(chr$(13) + str$(total) + " files are supposed to be located" +_
      " here." + chr$(13))
   counter = 1
   do
      get #8, (512 * arealist(resetarea)) - 512 + counter
      fileloc = cvi(filenum$)
      if fileloc = 0 then call prt(chr$(13) + "End of files." + chr$(13)) :_
         decr counter : exit loop
      call prt(chr$(13) + "File #" + str$(counter) + ": ")
      get #5, fileloc
      call unpad(fdname$,temp$)
      fsname$ = space$(12)
      lset fsname$ = temp$
      fssize$ = space$(5)
      rset fssize$ = str$(cvi(fdsize$))
      call unpad(fddesc$,fsdesc$)
      filename$ = bbspfx$ + "FILES\" + fsname$
      if fnfileexist(filename$) = 1_
         then found$ = "(FOUND)"_
         else found$ = "(NOT FOUND)"
      call prt(fsname$ + " :" + fssize$ + ": " + fsdesc$ +_
         "  " + found$)
      if val(fssize$) = 0_
         then call prt(" - Null file.  Renaming file..." + chr$(13)) :_
              lset fdname$ = right$(str$(counter),len(str$(counter)) - 1)_
                 + ".NUL" :_
              lset fddesc$ = "*** Empty file - delete ***"
              put #5, fileloc
      call prt(chr$(13))
      incr counter
   resetfilesloop:
   loop
   lset filenum$ = mki$(counter)
   put #8, 512 * arealist(resetarea)
   call prt(chr$(13) + str$(counter) + " files were actually found." +_
      chr$(13))
   goto filefunctions

