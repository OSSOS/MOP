#!/bin/bash

for expnum in `vls vos:OSSOS/dbimages` ; do  
    uri=vos:OSSOS/dbimages/${expnum}
    tagstr=`vtag ${uri} | grep canfar | grep -v ossos | awk -F\' '{ printf("%s= ", $2) } ' `
    len=${#tagstr}
    if [ "$len" -gt 0 ]; then 
	echo vtag ${uri} ${tagstr}
    fi
done