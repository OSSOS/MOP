#!/bin/bash

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc


expnum=$1
echo "Working on ${expnum}"

# Test that we need to do the job
vls vos:OSSOS/dbimages/${expnum}/${expnum}p.fits >& /dev/null && echo "Already did ${expnum}" && exit

# prep this image for the ossos pipeline
preproc.py --overscan --flat 13AQ05_r_flat.fits --short --verbose $expnum 
update_header.py -v --replace ${expnum}
vcp -v ${expnum}p.fits vos:OSSOS/dbimages/${expnum}/
mkpsf.py $expnum -v
step1.py $expnum -v

