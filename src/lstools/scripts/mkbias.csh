#!/bin/csh

#make combined  bias images 

set ddd=`pwd`
echo $ddd
foreach i (`cat chips`)
 if (-e ttt) /bin/rm ttt
 ls *b_$i.fits > ttt
 cd ~/iraf
cl << EOF 
pwd
cd $ddd 
pwd
imred
ccdred
zerocombine input=@ttt output=bias_$i.fits combine=median reject=minmax scale=none nlow=1 nhigh=3 nkeep=2
logout
EOF
cd $ddd
end



