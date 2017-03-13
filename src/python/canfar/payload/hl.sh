#!/bin/bash
# Run the Moving Object Pipeline on the given exposure numbers
source ${HOME}/.bash_profile

export DBIMAGES=vos:jkavelaars/HL/
export MEASURE3=vos:jkavelaars/HL/cands/
export rmax=6.6
export rmin=0.5
export ang=0.0
export width=90

basedir=`pwd`

for ccd in {0..35} 
  do
  mkdir ${ccd}
  cd ${ccd}
  # First scramble the images.
  scramble.py $1 $2 $3 --ccd $ccd -v --dbimages ${DBIMAGES} --force

  # now run the standard pipeline on the scramble images..
  mkpsf.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES} --force --ignore-update-headers
  step1.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES} --force
  step2.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES} --force
  step3.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${width} --force
  combine.py $1 --ccd $ccd --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3}  --force  

  # Now plant artificial sources.
  plant.py $1 $2 $3 --ccd $ccd -v --dbimages ${DBIMAGES} --type s --rmin ${rmin} --rmax ${rmax} --ang ${ang} --width ${width} --force

  # Now run the standard pipeline on the artificial sources.
  mkpsf.py $1 $2 $3 --ccd $ccd --fk -v --type s --dbimages ${DBIMAGES} --force --ignore-update-headers
  step1.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES} --force
  step2.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES} --force
  step3.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${width} --force
  combine.py $1 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3}  --force  

  # compute the variation in magnitudes from planeted images
  astrom_mag_check.py fk$1s*${ccd}.measure3.cands.astrom  --dbimages ${DBIMAGES}
  vcp fk$1s*${ccd}.measure3.cands.match ${MEASURE3}
  cd ${basedir}
done
