#!/bin/csh

# Need list of chips (01 02 03 ... 36) - chips (made here)
# This also makes "fields" file, although not necessary till next step moveim
#   (just easier while images are in predictable format)
# You'll end up with a lot of images in this same directory -
# the original images will remain, plus split images (which may
# be bias-subtracted or flat-fielded if left alone here) and flat images

# SO - altogether steps to running pipeline:
# 0 make "need files" above with makenfiles.sc
# 1 do the things here (preproc.sc) to the raw data 
#    (need bias frame and make flat before run last flatfield step- 
#    imcombine biased images, imarith
#    to give flat mean of 1 (imstat / mean))
# 2 then find image field names ("fields") - FILENAME OBJECT
# 3 then run moveim.sc to put images in proper directory
# 4 then run setuppipe.sc to generate jmpmakepsf input and start jmpmakepsf
# 5 then pipeline runs automatically (?)


#initial processing
# this only overscan subtracts and does flip of chip 1-18
foreach i (`cat imlist`)
 procmega $i.fits -p -u
 procmega $i.fits -s
# /bin/rm $i.fits    
end
