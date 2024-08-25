print "Black Hole subboard pointer rebuilding program."
print

open "SUBBOARD.IDX" as #1 len = 2
   field #1, 2 as msgnum$

do
   input "Enter subboard #";postboard
   if postboard = 0 then exit loop
   input "Enter message #";msgptr

   get #1, 256 * postboard
   postloc = cvi(msgnum$)
   incr postloc
   lset msgnum$ = mki$(msgptr)
   put #1, (256 * postboard) - 256 + postloc
   lset msgnum$ = mki$(postloc)
   put #1, 256 * postboard
   print "Record #";postloc;" for subboard #";postboard;" written."
   print
loop

close #1
end

