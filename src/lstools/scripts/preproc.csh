#!/bin/csh

#definitely need to put bias name in
# give bias as argument one flat as argument 2

# Need list of image root names - imlist (made here)
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
 procmega $i.fits -o -u
# /bin/rm $i.fits    
end

# then run bias subtraction
foreach j  (`cat chips`)
  foreach i (`cat imlist`)
    procmega $i"_"$j.fits -b bias_$j.fits
#   procmega $i"_"$j.fits -b $1"_"$j.fits
 end
end
# substitute your own bias field above

# make flat field images
set ddd=`pwd`
echo $ddd
if (-e sss) /bin/rm sss
touch sss
foreach i (`cat chips`)
 if (-e ttt) /bin/rm ttt
 ls *o_$i.fits > ttt    
 cd ~/iraf
cl << EOF
pwd    
cd $ddd
pwd
imcombine input=@ttt output=hflat_$i.fits combine=median reject=minmax outtype=ushort scale=mode zero=none weight=none nlow=3 nhigh=20 nkeep=5
# imstat hflat_$i.fits fields=image,mean format=no >> sss
imscale input=hflat_$i.fits output=flat_$i.fits mean=1 
##del hflat_$i.fits
logo
EOF
cd $ddd
end

# then do flat fielding - remember, have to have flat with
# mean value of one 
#foreach j (`cat chips`)
#  foreach i (`cat imlist`)
#   echo procmega $i"_"$j.fits -f flat_$j.fits
#   procmega $i"_"$j.fits -f flat_$j.fits
# end
#end
