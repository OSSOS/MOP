#!/bin/bash
# Run the Moving Object Pipeline on the given exposure numbers

ccd=$4
force=$5
## First do the search images
mkpsf.py $1 $2 $3 --ccd $ccd -v  ${force}
step1.py $1 $2 $3 --ccd $ccd -v  ${force}
step2.py $1 $2 $3 --ccd $ccd -v  ${force}
step3.py $1 $2 $3 --ccd $ccd -v  ${force}
combine.py $3 -v  --ccd $ccd ${force}

## Now build a scramble set and search
scramble.py $1 $2 $3 --ccd $ccd -v  ${force}
plant.py $1 $2 $3 --ccd $ccd -v ${force}
step1.py $1 $2 $3 --ccd $ccd --fk --type s -v  ${force}
step2.py $1 $2 $3 --ccd $ccd --fk --type s -v  ${force}
step3.py $1 $2 $3 --ccd $ccd --fk --type s -v ${force}
combine.py $3 --ccd $ccd --fk --type s -v  ${force}
