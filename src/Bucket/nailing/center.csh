#!/bin/csh

echo "You are using center.csh.  This script has hardwired values good for CFHTLS"

set workdir = `pwd`

set image = $1
echo $image 

set input = $2
echo $input 

set output = $3
echo $output


cd ${HOME}/iraf

cl << EOF

digiphot

daophot

cd $workdir

s1 = mktemp("$output")

if ( access("$output") ) {
	delete("$output",verify-,go_ahead+,allversions+,subfiles+) ;
;
}
;


imgets("$image","MAXLIN")
x = real(imgets.value)


photpars.apertures=15.0

photpars.zmag=26.0

datapars.datamin=INDEF
datapars.datamax=30000
datapars.exposur="EXPTIME"

centerpars.calgori="centroid" 

phot("$image","$input",s1,verify-,update-,verbose-)

pdump (s1,"XCENTER,YCENTER,MAG,MERR","MERR != INDEF && MAG != INDEF && PIER==0", header-,parameters+,  > "$output")

#delete(s1,verify-,go_ahead+,allversions+,subfiles+) 
#
logout

EOF

echo "center exits here"

exit


