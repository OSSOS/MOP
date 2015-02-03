#!/bin/csh
# JJK January 2015
# Just plant source into an image based on content of Object.planted file.

# WARNING: this script is obsolete and has been replace by ossos/plant.py

set wdir = $1

set dir = `pwd`

# match the .phot files left behind by the makepsf script.

# go into IRAF and do the planting.
set term = none

cd ~/iraf

cl -old <<EOF

flpr

kinky

pipeline

cd $dir 

cd $wdir

pipeplant proc-these-files Object.planted verbose+

touch plant.csh.OK

logout

EOF

cd $dir
