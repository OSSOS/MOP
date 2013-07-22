#!/bin/bash

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

expnum=$1
echo "Working on ${expnum}"

# Test that we need to do the job
preproc.py --overscan --flat 13AQ05_r_flat.fits --short --verbose $expnum  || exit -1
vcp ${expnum}p.fits vos:OSSOS/dbimages/${expnum}/${expnum}p.fits


