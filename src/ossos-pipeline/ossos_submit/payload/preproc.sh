#!/bin/bash

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

expnum=$1
echo -n "Checking ${expnum}; "

# Test that we need to do the job
status=`vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_36`

if [ ${status} != "'success'" ]
then
  vls vos:OSSOS/dbimages/${expnum}/${expnum}p.fits >& /dev/null && vtag vos:OSSOS/dbimages/${expnum} 'ivo://canfar.uvic.ca/ossos#preproc_o36=success'
  status=`vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_o36`
fi

echo -n " status = ${status} -> "

flat=13AQ05_r_flat.fits
#flat=13B_r_flat.fits

if [ ${status} != "'success'" ]
then
  echo "Running preproc on ${expnum} using flat 13B_r_flat.fits"
  # 13A has 13AQ05_r_flat.fits
  preproc.py --overscan --flat ${flat} --short --verbose $expnum  || exit -1
  vcp ${expnum}p.fits vos:OSSOS/dbimages/${expnum}/${expnum}p.fits
  vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_o36=success
  rm ${expnum}p.fits
  rm ${expnum}o.fits.fz
else
  echo "Skipping  ${expnum} "
fi

