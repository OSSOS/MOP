#!/bin/csh
foreach i (`cat list.splice`)
imsplice $i temp.fits
/bin/mv temp.fits $i
end
