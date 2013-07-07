#!/bin/bash
source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

ls

mkpsf.py -v $1 --ccd $2

