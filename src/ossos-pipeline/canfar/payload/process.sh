#!/bin/bash
# Run the Moving Object Pipeline on the given exposure numbers
source ${HOME}/.bash_profile

export DBIMAGES=vos:OSSOS/dbimages/
export MEASURE3=vos:OSSOS/measure3/2013A-E_April9/E-2+0/
# rmin, rmax are constants for the whole OSSOS survey.
export rmax=15.0
export rmin=0.5
# -23 for L block. +20 for E block.
export ang=20
# width has been constant for a while now
export width=30
export field=$4
export ccd_start=$5
export ccd_end=$6
export force=
basedir=`pwd`

for ((ccd=ccd_start;ccd<=ccd_end;ccd++))
  do
  mkdir ${ccd}
  cd ${ccd}
  ## First do the search images
  mkpsf.py $1 $2 $3 -v --ccd ${ccd} ${force}
  step1.py $1 $2 $3 -v --ccd ${ccd} ${force}
  step2.py $1 $2 $3 -v --ccd ${ccd} ${force}
  step3.py $1 $2 $3 --ccd $ccd  -v --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${width}  
  echo "Running combine.py"
  combine.py $1 -v --measure3 ${MEASURE3} --field ${field}  --ccd ${ccd} ${force}

  # First scramble the images.
  scramble.py $1 $2 $3 --ccd $ccd -v --dbimages ${DBIMAGES}  ${force}

  # now run the standard pipeline on the scramble images..
  mkpsf.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES}  ${force}
  step1.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES}  ${force}
  step2.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES}  ${force}
  step3.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${width} ${force}
  combine.py $1 --ccd $ccd --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --field ${field} ${force}

  # Now plant artificial sources.
  plant.py $1 $2 $3 --ccd $ccd -v --dbimages ${DBIMAGES} --type s --rmin ${rmin} --rmax ${rmax} --ang ${ang} --width ${width}  ${force}

  # Now run the standard pipeline on the artificial sources.
  mkpsf.py $1 $2 $3 --ccd $ccd --fk -v --type s --dbimages ${DBIMAGES} --type s ${force}
  step1.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES}  ${force}
  step2.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES}  ${force}
  step3.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${width}  ${force}
  combine.py $1 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} ${force} --field ${field}

  # compute the variation in magnitudes from planeted images
  astrom_mag_check.py fk$1s*${ccd}.measure3.cands.astrom  --dbimages ${DBIMAGES} 
  vcp fk$1s*${ccd}.measure3.cands.match ${MEASURE3} 
  cd ${basedir}
done
