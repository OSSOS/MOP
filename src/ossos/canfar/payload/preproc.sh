#!/bin/bash

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

expnum=$1
echo -n "Checking ${expnum}; "
status="FAILED"
## uncomment this tag if you want to 'force' a complete re-preproc
vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_36=
# Test that we need to do the job
status=`vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_o36`

#if [ ${status} != "'success'" ]
#then
#  vls vos:OSSOS/dbimages/${expnum}/${expnum}p.fits >& /dev/null && vtag vos:OSSOS/dbimages/${expnum} 'ivo://canfar.uvic.ca/ossos#preproc_o36=success'
#  status=`vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_o36`
#fi

echo " ${expnum} status = ${status} -> "

# 13A has 13AQ05_r_flat.fits
#flat=13AQ05_r_flat.fits
## this is the one that is full of 1s flat=13B_r_flat.fits
## new flat for 13B: 13B_r_flat_2.fits
flat=LOOKUP
echo " flat = ${flat} "

if [ ${status} != "'success'" ]
then
  echo "Running preproc on ${expnum} using flat ${flat}"
  preproc.py --overscan --flat ${flat} --short --megapipe --verbose $expnum  || exit -1
  vcp ${expnum}p.fits vos:OSSOS/dbimages/${expnum}/${expnum}p.fits || exit -1
  vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_o36=success
  rm ${expnum}p.fits
  rm ${expnum}o.fits.fz
else
  echo "Skipping  ${expnum} "
fi

