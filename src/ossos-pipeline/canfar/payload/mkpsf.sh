#!/bin/bash

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

expnum=$1
mkpsf.py $expnum -v

