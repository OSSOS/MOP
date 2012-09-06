#!/bin/csh

#definitely need to put bias name in
# give bias as argument one flat as argument 2

# Need list of image root names - imlist 
# Need list of chips (01 02 03 ... 36) - chips 
# You'll end up with a lot of images in this same directory -
# the original images will remain, plus split images (which may
# be bias-subtracted or flat-fielded if left alone here) and flat images

foreach i (`cat imlist`)
 (if -e $i.fz) mv $i.fz $i
end

#initial processing for RAW images
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
imscale input=hflat_$i.fits output=flat_$i.fits mean=1 
del hflat_$i.fits
logo
EOF
cd $ddd
end


# now actually do flat fielding - remember, have to have flat with
# mean value of one 
foreach j (`cat chips`)
  foreach i (`cat imlist`)
   echo procmega $i"_"$j.fits -f flat_$j.fits
   procmega $i"_"$j.fits -f flat_$j.fits
 end
end

ls *_??.fits |grep -v flat |grep -v bias > shortlist
set ddd=`pwd`
echo $ddd
cd ~/iraf
cl << EOF
pwd
cd $ddd
pwd
chpixtype @shortlist @shortlist ushort
EOF
cd $ddd

echo "done"
