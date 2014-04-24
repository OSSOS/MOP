#!/bin/bash
# Run the Moving Object Pipeline on the given exposure numbers
source ${HOME}/.bash_profile

export DBIMAGES=vos:jkavelaars/HL/
export MEASURE3=vos:jkavelaars/HL/cands/

exp=$1


for ccd in {0..35} 
do

## First do the search images
scramble.py $1 $2 $3 --ccd $ccd -v --dbimages ${DBIMAGES}
mkpsf.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES}
step1.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES}
step2.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES}
step3.py $1 $2 $3 --ccd $ccd -v --type s --dbimages ${DBIMAGES}
## Now build a scramble set and search
plant.py $1 $2 $3 --ccd $ccd -v --dbimages ${DBIMAGES}
step1.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES}
step2.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES}
step3.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES}
combine.py $1 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3}  --force
/home/jkavelaars/MOP/src/ossos-pipeline/astrom_mag_check.py ${MEASURE3}fk_${exp}_s${ccd}.measure3.cands.astrom  --dbimages ${DBIMAGES}

done
