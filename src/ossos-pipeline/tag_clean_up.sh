#!/bin/bash

## this script was run to cleanup tags that had been writen prior to JJ's choice
## of what the tag uri would be stabilizing. 


# remove all tags that claim to be from 'canfar' but not 'ossos'.  

for expnum in `vls vos:OSSOS/dbimages` ; do  
    uri=vos:OSSOS/dbimages/${expnum}
    tagstr=`vtag ${uri} | grep canfar | grep -v ossos | awk -F\' '{ printf("%s= ", $2) } ' `
    len=${#tagstr}
    if [ "$len" -gt 0 ]; then 
	echo vtag ${uri} ${tagstr}
    fi
done



## remove all non-version tags from a dbimages/exposure container if that container has a versioned mkpsf tag. which indicates that
## new style tags should exist for that exposure

for expnum in `vls vos:OSSOS/dbimages/` ; do
    [ `vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#mkpsf_p00` == 'None' ] || echo $expnum ;
     done
> version_processed.txt

cat version_processed.txt | while read exp ; do vtag vos:OSSOS/dbimages/${exp} | grep "_\d\d\'" | grep -v preproc >& /dev/null && echo $exp ; done > double.dat

cat double.dat | while read exp ; do vtag vos:OSSOS/dbimages/${exp} | grep "_\d\d\'" | grep -v preproc | awk ' { print substr($1,1,length($1) -1) } ' | while read uri ; do echo "vtag vos:OSSOS/dbimages/${exp} ${uri}= " ; done ; done  | /bin/bash


