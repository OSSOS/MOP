#!/bin/csh

set workdir = `pwd`

set image = $1

set fwhm = $2

set maxcount = $3

set zeropt = $4

set exptime = $5

set x = $6

set y = $7


cd ${HOME}/iraf

cl << EOF

digiphot

daophot

cd $workdir

s1 = mktemp("mag")

if ( access(s1) ) {
	delete(s1,verify-,go_ahead+,allversions+,subfiles+) ;
;
}
;

;
#photpars.apertures=$fwhm*0.9
#photpars.zmag=$zeropt
#datapars.datamin=INDEF
#datapars.datamax=$maxcount
#datapars.itime=$exptime

#centerpars.calgori="centroid"  

s2 = mktemp("coo")

print("$x $y ",> s2)

phot("$image",s2,s1,verify-,update-,verbose-, photpars.apertures=$fwhm*0.9, photpars.zmag=$zeropt, datapars.datamin=INDEF, datapars.datamax=$maxcount, datapars.itime=$exptime )

txdump (s1,"XCENTER,YCENTER,MAG,MERR","yes", header-)  | scan(line)


delete(s2,verify-,go_ahead+,allversions+,subfiles+) 

if ( access(s1) ) {
	delete(s1,verify-,go_ahead+,allversions+,subfiles+) ;
;
}
;

print ("\n\n")
print ("DOPHOT ",line)
print ("\n\n")

logout


EOF

exit
