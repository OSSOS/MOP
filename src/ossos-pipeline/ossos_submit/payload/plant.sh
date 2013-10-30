#!/bin/bash
# Run the Moving Object Pipeline on the given exposure numbers
source ${HOME}/.bash_profile
DBIMAGES=vos:jkavelaars/HL
ccd=$4

## Now build a scramble set and search
scramble.py $1 $2 $3 --ccd $ccd -v  --dbimages ${DBIMAGES}
mkpsf.py --type s $1 $2 $3 --ccd $ccd -v --dbimages ${DBIMAGES} 
plant.py $1 $2 $3 --ccd $ccd -v --dbimages ${DBIMAGES}
step1.py $1 $2 $3 --ccd $ccd --fk --type s -v --dbimages ${DBIMAGES}
step2.py $1 $2 $3 --ccd $ccd --fk --type s -v   --dbimages ${DBIMAGES}
step3.py $1 $2 $3 --ccd $ccd --fk --type s -v  --dbimages ${DBIMAGES}
echo "Running combine.py"
echo combine.py $1 --ccd $ccd --fk --type s -v   --dbimages ${DBIMAGES}
combine.py $1 --ccd $ccd --fk --type s -v   --dbimages ${DBIMAGES}
