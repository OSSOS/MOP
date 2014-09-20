#!/bin/bash

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

expnum=$1
echo "Working on ${expnum}"

update_header.py -v --replace ${expnum}

mkpsf.py $expnum -v
step1.py $expnum -v

