#!/bin/bash

if [ -f ${HOME}/.bashrc ]
then
   source ${HOME}/.bashrc
fi

export exp2=1667740
export exp3=1667751
export exp1=1667729

if [ ! -d "TEST" ] 
then
# Create a clean testing area
    vrmdir vos:OSSOS/TEST/dbimages
    vmkdir vos:OSSOS/TEST/dbimages
    vmkdir vos:OSSOS/TEST/dbimages/measure3
    
# link Test files into new area
    for exp in ${exp1} ${exp2} ${exp3}
    do
	vmkdir vos:OSSOS/TEST/dbimages/${exp}
	vln vos:OSSOS/dbimages/${exp}/${exp}p.fits  vos:OSSOS/TEST/dbimages/${exp}/${exp}p.fits
    done
    vln vos:OSSOS/dbimages/calibrators vos:OSSOS/TEST/dbimages/calibrators
fi

export DBIMAGES=vos:OSSOS/TEST/dbimages
export MEASURE3=${DBIMAGES}/measure3/
# rmin, rmax are constants for the whole OSSOS survey.
export rmax=15.0
export rmin=0.5
# -23 for L block. +20 for E block.
export ang=-23
# width has been constant for a while now
export width=30
export field=TEST
export ccd_start=0
export ccd_end=0
export number=20
export loops=1
export force=
#export force=--force

echo "field "${field}

mkdir -p ${field}
cd ${field}
basedir=`pwd`
for expnum in ${exp1} ${exp2} ${exp3}
  do
    update_header.py --replace ${expnum} --dbimages ${DBIMAGES}
  done

for ((ccd=ccd_start;ccd<=ccd_end;ccd++))
do
  mkdir -p ${ccd}
  cd ${ccd}
  ## First do the search images
  mkpsf.py ${exp1} ${exp2} ${exp3} -v --ccd ${ccd} ${force} --dbimages ${DBIMAGES}
  step1.py ${exp1} ${exp2} ${exp3} -v --ccd ${ccd} ${force} --dbimages ${DBIMAGES}
  step2.py ${exp1} ${exp2} ${exp3} -v --ccd ${ccd} ${force} --dbimages ${DBIMAGES}
  step3.py ${exp1} ${exp2} ${exp3} --ccd ${ccd}  -v --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${width} ${force}
  echo "Running combine.py"
  combine.py ${exp1} -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --field ${field}  --ccd ${ccd} ${force}

  # First scramble the images.
  scramble.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --dbimages ${DBIMAGES}  ${force}

  # now run the standard pipeline on the scramble images..
  mkpsf.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES}  ${force} --ignore-update-headers
  step1.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES}  ${force}
  step2.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES}  ${force}
  step3.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${width} ${force}
  combine.py ${exp1} --ccd ${ccd} --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --field ${field} ${force}

  # Now plant artificial sources.
  for ((loop=1;loop<=${loops};loop++)) 
  do
      align.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --dbimages ${DBIMAGES} --type s
      plant.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --dbimages ${DBIMAGES} --type s --rmin ${rmin} --rmax ${rmax} --ang ${ang} --width ${width} --number ${number}

  # Now run the standard pipeline on the artificial sources.
     mkpsf.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk -v --type s --dbimages ${DBIMAGES} --type s ${force} --ignore-update-headers
     plant.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --dbimages ${DBIMAGES} --type s --rmin ${rmin} --rmax ${rmax} --ang ${ang} --width ${width}  --force
     step1.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force
     step2.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force
     step3.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${width}
     combine.py ${exp1} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --force --field ${field}
     astrom_mag_check.py ${field} ${ccd} --measure3 ${MEASURE3}  --dbimages ${DBIMAGES}  --force
  done

  cd ${basedir}
done
