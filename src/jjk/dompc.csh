#!/bin/csh
# JJK Dec 2002
##
##  run the dompc cl script in the current directory.


set idir = "~/iraf"
set wdir = `pwd`

cd $idir
set term = none

cl << EOF

flpr

kinky
pipeline

cd $wdir 

dompc proc-these-files 

logout

EOF

cd $wdir

