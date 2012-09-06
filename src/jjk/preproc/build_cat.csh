#!/bin/csh

jmpmakepsf.csh ./ $1 YES
set fwhm=`grep $1 proc-these-files | awk '{print ( $2 ) } ' `
step1jmp -f $1 -t 2.7 -w $fwhm -m 65000 
daophot.py --input $1.obj.jmp --output $1.mag.jmp $1
setp1matt -f $1 -t 1.3 -w $fwhm -m 65000 
daophot.py --input $1.obj.matt --output $1.mag.matt
merge.py $1.mag $1.obj.jmp $1.obj.matt $1
objIngest $1.mag


