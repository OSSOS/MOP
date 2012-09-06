#!/bin/csh

#definitely need to put bias name in
# give bias as argument one flat as argument 2

# Need list of image root names - imlist 
# Need list of chips (01 02 03 ... 36) - chips 
# You'll end up with a lot of images in this same directory -
# the original images will remain, plus split images (which may
# be bias-subtracted or flat-fielded if left alone here) and flat images


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
