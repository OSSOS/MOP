#!/bin/csh

# Processing and splitting for PROCESSED images

foreach i (`cat imlist`)
 if ( -e $i.fz) mv $i.fz $i
end

#initial processing for PROCESSED images
# this only splits images does flip of chip 1-18
foreach i (`cat imlist`)
 procmega $i.fits -p -u
# /bin/rm $i.fits    
end

# and now do rename of images, due to silly CFHT 

foreach i (`cat imlist`)
 foreach j (`cat chips`)
  mv $i$j"_"$j".fits" $i"_"$j".fits"
 end
end

