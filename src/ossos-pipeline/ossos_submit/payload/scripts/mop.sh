#!/bin/bash
source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

cp cadcproxy.pem /home/jkavelaars/.ssl/


for expnum in $1 $2 $3 ; 
do
   vls vos:OSSOS/dbimages/${expnum}/${expnum}p.fits >& /dev/null && continue
   ${HOME}/preproc.py --overscan --flat 13AQ05_r_flat.fits --short --verbose $expnum 
   vcp -v ${expnum}p.fits vos:OSSOS/dbimages/${expnum}/
done

#update_header.py -v --replace $1


for ccd in {0..35}; do
  DD=`pwd`
  mkdir $ccd 
  cd $ccd
## First do the search images
  mkpsf.py $1 $2 $3 --ccd $ccd -v 
  step1.py $1 $2 $3 --ccd $ccd -v 
  step2.py $1 $2 $3 --ccd $ccd -v 
  step3.py $1 $2 $3 --ccd $ccd -v 
  combine.py $3 -v 

## Now build a scramble set and search
  scramble.py $1 $2 $3 --ccd $ccd -v 
  plant.py $1 $2 $3 --ccd $ccd -v
  step1.py $1 $2 $3 --ccd $ccd --fk --type s -v 
  step2.py $1 $2 $3 --ccd $ccd --fk --type s -v 
  step3.py $1 $2 $3 --ccd $ccd --fk --type s -v 
  combine.py $3 --ccd $ccd --fk --type s -v 
  cd ${DD}
done
