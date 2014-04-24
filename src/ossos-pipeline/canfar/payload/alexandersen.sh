#!/bin/bash
source /home/jkavelaars/.bash_profile
#source /home/jkavelaars/.moprc

cp cadcproxy.pem /home/jkavelaars/.ssl/

dbimages=vos:jkavelaars/Alexandersen
measure3=vos:jkavelaars/Alexandersen/measure3

field=$4

#for expnum in $1 $2 $3 ; 
#do
#   vmkdir ${dbimages}/${expnum}
#   vls ${dbimaegs}/${expnum}/${expnum}p.fits >& /dev/null && continue
#   preproc.py --overscan --flat 13AQ05_r_flat.fits --short --verbose $expnum  --dbimages ${dbimages}
#   vcp -v ${expnum}p.fits ${dbimages}/${expnum}/
#done

basedir=`pwd`

for ccd in {0..35}; do

  mkdir $ccd 
  cd $ccd

  ## First do the search images
  mkpsf.py $1 $2 $3 --ccd $ccd -v 
  step1.py $1 $2 $3 --ccd $ccd -v --dbimages ${dbimages}
  step2.py $1 $2 $3 --ccd $ccd -v --dbimages ${dbimages}
  step3.py $1 $2 $3 --ccd $ccd -v --dbimages ${dbimages} --rmin 0.5 --rmax 10.0 --ang -22 -awidth 22
  combine.py $3 -v --dbimages vos:OSSOS/Alexandersen --measure3 ${measure3} --field ${4}

  ## Now build a scramble set and search
  scramble.py $1 $2 $3 --ccd $ccd -v --dbimages ${dbimages}
  plant.py $1 $2 $3 --ccd $ccd -v --dbimages ${dbimages} --rmin 0.5 --rmax 10.0 --ang -22 --aweith 21
  step1.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${dbimages}
  step2.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${dbimages}
  step3.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${dbimages} --rmin -0.5 --rmax 10.0 --ang -22 --awidth 21
  combine.py $3 --ccd $ccd --fk --type s -v --dbimages ${dbimages} --measure3 ${measure3} --field ${4}
  cd ${basedir}
done
