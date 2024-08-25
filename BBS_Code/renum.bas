'Black Hole BBS message base renumbering program
'By Brad Hufnagel
'----------------------------------------------------------------------------

print "Black Hole BBS message base renumbering program"
print

dim msgptr(256 * 48)

counter = 0

open "SUBBOARD.IDX" as #1 len = 2
   field #1, 2 as ptrsub$

print "Reading all subboards..."
for subboard = 1 to 48
   print "Board #";subboard;" -";
   get #1, 256 * subboard
   msgtotal = cvi(ptrsub$)
   counter = counter + msgtotal
   print msgtotal;" total messages"
   print
   for location = 1 to 255
      ptrloc = (256 * subboard) - 256 + location
      get #1, ptrloc
      if location <= msglimit_
         then msgptr(ptrloc) = cvi(ptrsub$)_
         else msgptr(ptrloc) = 0
   next location
next subboard
close #1

print
print counter;" total messages read in from subboards."
end
