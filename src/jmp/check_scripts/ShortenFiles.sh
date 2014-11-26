#! /bin/bash

for c in chip??; do
    cd $c
    for f in *; do
	cd $f
	for i in *.fits; do
	    size=`wc -c $i | cut -d " " -f 1`
	    if [ ${size} -gt 19000000 ]; then
		chpix $i tmp.fits ushort
		\mv tmp.fits $i
	    fi
	done
	cd ../
    done
    cd ../
done

exit
