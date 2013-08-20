#!/bin/bash
# This script laysout all the steps needed to run the OOSOS detection pipeline
# on a triple of OSSOS discovery images
#
# Usage:  mop.sh exp1 exp2 exp3 
#
FLAT=13AQ05_r_flat.fits
source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

-f cadcproxy.pem && cp cadcproxy.pem ${HOME}/.ssl/

## du image preprocessing
for expnum in $1 $2 $3 ; 
do
   vls vos:OSSOS/dbimages/${expnum}/${expnum}p.fits >& /dev/null && continue
   ${HOME}/preproc.py --overscan --flat ${FLAT} --short --verbose $expnum 
   update_header.py -v --replace $1
   vcp -v ${expnum}p.fits vos:OSSOS/dbimages/${expnum}/
done


###
### now loop over each chip, doing each one in its own directory 
### to avoid name collisions.
###
DD=`pwd`
for ccd in {0..35}; do
  mkdir $ccd 
  cd $ccd
## First do the search images
  mkpsf.py $1 $2 $3 --ccd $ccd -v 
  step1.py $1 $2 $3 --ccd $ccd -v 
  step2.py $1 $2 $3 --ccd $ccd -v 
  step3.py $1 $2 $3 --ccd $ccd -v 
  combine.py $1 -v 

## Now build a scramble set and search
  scramble.py $1 $2 $3 --ccd $ccd -v 
  plant.py $1 $2 $3 --ccd $ccd -v
  step1.py $1 $2 $3 --ccd $ccd --fk --type s -v 
  step2.py $1 $2 $3 --ccd $ccd --fk --type s -v 
  step3.py $1 $2 $3 --ccd $ccd --fk --type s -v 
  combine.py $3 --ccd $ccd --fk --type s -v 
  cd ${DD}
done
