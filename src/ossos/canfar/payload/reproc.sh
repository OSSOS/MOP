#!/bin/bash

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

expnum=$1
echo -n "Checking ${expnum}; "


# delete the previous preproc tag
vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_36=

  echo "Running preproc on ${expnum}"
  preproc.py --overscan --flat 13AQ05_r_flat.fits --short --verbose $expnum  || exit -1
  # update the header and copy to VOspace
  update_header.py  --verbose --force --replace ${expnum}
  vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#preproc_36=success
  rm ${expnum}p.fits
  rm ${expnum}o.fits.fz
  rm ${exonum}p.head

