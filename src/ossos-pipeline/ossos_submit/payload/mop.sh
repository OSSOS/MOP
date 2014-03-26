#!/bin/bash
# Run the Moving Object Pipeline on the given exposure numbers
source ${HOME}/.bash_profile

field=$4
ccd=$5


## First do the search images
mkpsf.py $1 $2 $3 --ccd $ccd -v
step1.py $1 $2 $3 --ccd $ccd -v
step2.py $1 $2 $3 --ccd $ccd -v
step3.py $1 $2 $3 --ccd $ccd -v
echo "Running combine.py"
combine.py $1 -v  --ccd $ccd --measure3 vos:OSSOS/measure3/2013B-H --field ${field}

## Now build a scramble set and search
echo "scramble and plant"
scramble.py $1 $2 $3 --ccd $ccd -v
mkpsf.py $1 $2 $3 --ccd $ccd -v --type s
step1.py  $1 $2 $3 --ccd $ccd -v --type s
step2.py   $1 $2 $3 --ccd $ccd -v --type s
plant.py $1 $2 $3 --ccd $ccd -v --type s
step1.py $1 $2 $3 --ccd $ccd --fk --type s -v 
step2.py $1 $2 $3 --ccd $ccd --fk --type s -v 
step3.py $1 $2 $3 --ccd $ccd --fk --type s -v 
combine.py $1 --fk --type s -v --ccd $ccd --measure3 vos:OSSOS/measure3/2013B-H --field ${field}
