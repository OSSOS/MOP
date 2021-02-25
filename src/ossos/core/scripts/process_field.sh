#!/bin/bash

export exp1=2480215
export exp2=2480233
export exp3=2480258
export field=Field_1
export ccd_start=6
export ccd_end=6

export DBROOT=vos:peltierl/Kozai/
export asgwyn=$DBROOT/cfht_kozai
export DBIMAGES=$DBROOT/DBIMAGES/
export MEASURE3=$DBROOT/measure3/${field}

echo "$0 -- $exp1 -- $exp2 -- $exp3 -- $field -- $ccd_start -- $ccd_end "

vmkdir -p ${MEASURE3}
    
# rmin, rmax are constants for the whole OSSOS survey.
# rmin, rmax are constants for the whole OSSOS survey.
export rmax=15.0
export rmin=0.3
# -23 for L block. +20 for E block.
export ang=+12
# width has been constant for a while now
export search_width=20
export plant_width=10
export loops=15
export num=30
export force=--force
export force=
export dry_run=

echo "field "${field}

mkdir -p ${field}
cd ${field}
basedir=`pwd`

for expnum in ${exp1} ${exp2} ${exp3} 
do
   vls ${DBIMAGES}/${expnum} || ( vmkdir -p ${DBIMAGES}/${expnum} ; vln $asgwyn/${expnum}p.fits ${DBIMAGES}/${expnum}/${expnum}p.fits )
done 

for ((ccd=ccd_start;ccd<=ccd_end;ccd++))
do
    cd ${basedir}
    mkdir -p ${ccd}
    cd ${ccd}
    for ((loop=1;loop<=${loops};loop++))
    do
        ## First do the search images
	mk_mopheader ${exp1} ${exp2} ${exp3} -v --ccd ${ccd} ${force} --dbimages ${DBIMAGES} ${dry_run}
	mkpsf ${exp1} ${exp2} ${exp3} -v --ccd ${ccd} ${force} --dbimages ${DBIMAGES} ${dry_run}
	step1 ${exp1} ${exp2} ${exp3} -v --ccd ${ccd} ${force} --dbimages ${DBIMAGES} ${dry_run}
	step2 ${exp1} ${exp2} ${exp3} -v --ccd ${ccd} ${force} --dbimages ${DBIMAGES} ${dry_run}
	step3 ${exp1} ${exp2} ${exp3} --ccd ${ccd}  -v --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${search_width} ${force} ${dry_run}
	echo "Running combine"
	combine ${exp1} -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --field ${field}  --ccd ${ccd} ${force} ${dry_run}
	
        # First scramble the images.
	scramble ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --dbimages ${DBIMAGES}  ${force} ${dry_run}
	
        # now run the standard pipeline on the scramble images..
	mkpsf ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES}  ${force} --ignore-update-headers ${dry_run}
	step1 ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES}  ${force} ${dry_run}
	step2 ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES}  ${force} ${dry_run}
	step3 ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${search_width} ${force} ${dry_run}
	combine ${exp1} --ccd ${ccd} --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --field ${field} ${force} ${dry_run}
	
	align ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --dbimages ${DBIMAGES} --type s ${force} ${dry_run}
	\rm fk*.fits
	plant ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --dbimages ${DBIMAGES} --type s --rmin ${rmin} --rmax ${rmax} --ang ${ang} --width ${plant_width} --num ${num} --force ${dry_run}
        # Now run the standard pipeline on the artificial sources.
	mkpsf ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk -v --type s --dbimages ${DBIMAGES} --type s ${force} --ignore-update-headers ${dry_run}
	step1 ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force ${dry_run}
	step2 ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force ${dry_run}
	step3 ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${search_width} ${dry_run}
	combine ${exp1} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --force --field ${field} ${dry_run}
	astrom_mag_check ${field} ${ccd} --measure3 ${MEASURE3}  --dbimages ${DBIMAGES}  --force ${dry_run}
        # Only allow the manual force on the first loop through.
	force=
    done
    # one more time for stoage to VOSpace.
    \rm fk*.fits
    plant ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --dbimages ${DBIMAGES} --type s --rmin ${rmin} --rmax ${rmax} --ang ${ang} --width ${plant_width} --num ${num} --force 
    # Now run the standard pipeline on the artificial sources.
    mkpsf ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk -v --type s --dbimages ${DBIMAGES} --type s ${force} --ignore-update-headers
    step1 ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force
    step2 ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force
    step3 ${exp1} ${exp2} ${exp3} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --force --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${search_width}
    combine ${exp1} --ccd ${ccd} --fk --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --force --field ${field} 
    astrom_mag_check ${field} ${ccd} --measure3 ${MEASURE3}  --dbimages ${DBIMAGES}  --force 
done
