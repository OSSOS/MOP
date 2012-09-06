#!/bin/csh

# Need list of image root names - imlist (made here)
# Need list of chips (01 02 03 ... 36) - chips (made here)
# This also makes "fields" file, although not necessary till next step moveim
#   (just easier while images are in predictable format)
# You'll end up with a lot of images in this same directory -
# the original images will remain, plus split images (which may
# be bias-subtracted or flat-fielded if left alone here) and flat images

# SO - altogether steps to running pipeline:
# 0 make the files here
# 1 do the things here (preproc.sc) to the raw data 
#    (need bias frame and make flat before run last flatfield step- 
#    imcombine biased images, imarith
#    to give flat mean of 1 (imstat / mean))
# 2 then find image field names ("fields") - FILENAME OBJECT
# 3 then run moveim.sc to put images in proper directory
# 4 then run setuppipe.sc to generate jmpmakepsf input and start jmpmakepsf
# 5 then pipeline runs automatically (?)

# create field names with image root filename + OBJECT name ("fields")
if (-e fields) /bin/mv fields fields.o
set ddd=`pwd`
echo $ddd
cd ~/iraf
cl << EOF 
pwd
cd $ddd
pwd
hselect ???????.fits[0] FILENAME,OBJECT,DATE-OBS,UTC-OBS yes > fields
logo
EOF
cd $ddd
if (-e sss) /bin/rm sss
sed 's/.fits\[0\]//g' fields | sed 's/N//g' | sed 's/\"//g' | sed 's/Gladman//g' > sss
/bin/mv sss fields

# make "chips" file 
if (-e chips) /bin/rm chips
touch chips
foreach i (00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35)
 echo $i >> chips
end

# make "imlist" file
if (-e imlist) /bin/mv imlist imlist.o
<fields awk '{print $1}' > imlist

#make unique fields list (in fields repeated 3 times, b/c 3 images with same )
if (-e fieldsonly) /bin/rm fieldsonly
<fields awk '{print $2}' | sort |  uniq > fieldsonly

# so now have chips, imlist, fields, and fieldsonly with every object
# image included  .. 

# verify enough images for triplet and whether/not there are nailing obs
if (`<fields awk '{print $3}' | uniq |wc -l` > 1) then
 echo ""
 echo "Looks like there are nailing observations"
else
  echo "No nailings observations"
endif

if (-e imlist.nailing) /bin/mv imlist.nailing imlist.nailing.o
if (-e imlist.triplet) /bin/mv imlist.triplet imlist.triplet.o
if (-e imlist.other) /bin/mv imlist.other imlist.other.o
touch imlist.nailing
touch imlist.triplet
touch imlist.other
foreach i (`cat fieldsonly`)
 if (-e sss) /bin/rm sss
 if (-e sss2) /bin/rm sss2
 grep $i fields > sss
 <sss awk '{print $3}' | uniq > sss2
# are there any nailing obs at all?
 if (`<sss2 wc -l` > 1 ) then
  foreach i (`cat sss2`)
   if (-e sss3) /bin/rm sss3
   grep $i sss > sss3
   if (`<sss3 wc -l` == 3 ) then
    echo "hello" >> /dev/null
    <sss3 awk '{print $1}' >> imlist.triplet 
   else if (`<sss3 wc -l` == 1 )  then
    echo "hello" >> /dev/null
    <sss3 awk '{print $1}' >> imlist.nailing 
   else 
     echo "I need help with " `<sss awk '{print $2}'|uniq ` `<sss3 wc -l` `<sss3 awk '{print $1}'` 
     <sss3 awk '{print $1}' >> imlist.other
   endif
  end
 endif

# there is only one date, but enough images?
 if (`<sss2 wc -l` == 1 )  then
  if (`<sss wc -l` == 3 ) then 
    echo "hello" >> /dev/null
    <sss awk '{print $1}' >> imlist.triplet
  else
   <sss awk '{print $1}' >> imlist.other
     echo "I need help with " `<sss awk '{print $2}'|uniq ` `<sss wc -l` `<sss awk '{print $1}'` 
  endif
 endif

if (-e sss2) /bin/rm sss2 
if (-e sss3) /bin/rm sss3
if (-e sss) /bin/rm sss
end
