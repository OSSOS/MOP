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
  mkdir $ccd 
  cd $ccd
## First do the search images
  mkpsf.py $1 $2 $3 --ccd $ccd -v 
  step1.py $1 $2 $3 --ccd $ccd -v --dbimages vos:OSSOS/Alexandersen
  step2.py $1 $2 $3 --ccd $ccd -v --dbimages vos:OSSOS/Alexandersen
  step3.py $1 $2 $3 --ccd $ccd -v --dbimages vos:OSSOS/Alexandersen --rmin 0.5 --rmax 10.0 --ang -22 -awidth 22
  combine.py $3 -v --dbimages vos:OSSOS/Alexandersen

## Now build a scramble set and search
  scramble.py $1 $2 $3 --ccd $ccd -v --dbimages vos:OSSOS/Alexandersen
  plant.py $1 $2 $3 --ccd $ccd -v --dbimages vos:OSSOS/Alexandersen --rmin 0.5 --rmax 10.0 --ang -22 --aweith 21
  step1.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages vos:OSSOS/Alexandersen 
  step2.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages vos:OSSOS/Alexandersen
  step3.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages vos:OSSOS/Alexandersen --rmin -0.5 --rmax 10.0 --ang -22 --awidth 21
  combine.py $3 --ccd $ccd --fk --type s -v --dbimages vos:OSSOS/Alexandersen

cd ..
done
