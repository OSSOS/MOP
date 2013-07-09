#!/bin/bash
# Run the Moving Object Pipeline on the given exposure numbers

ccd=$4
## First do the search images
mkpsf.py $1 $2 $3 --ccd $ccd -v 
step1.py $1 $2 $3 --ccd $ccd -v 
step2.py $1 $2 $3 --ccd $ccd -v 
step3.py $1 $2 $3 --ccd $ccd -v 
combine.py $3 -v 

## Now build a scramble set and search
scramble.py $1 $2 $3 --ccd $ccd -v 
plant.py $1 $2 $3 --ccd $ccd -v
step1.py $1 $2 $3 --ccd $ccd --fk --type s -v 
step2.py $1 $2 $3 --ccd $ccd --fk --type s -v 
step3.py $1 $2 $3 --ccd $ccd --fk --type s -v 
combine.py $3 --ccd $ccd --fk --type s -v 
