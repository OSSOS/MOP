#!/bin/csh

# Script to "deterend" cfh12k data.  The only 12k specific step is
# runiging the "edhead" script (which is done in the cfh12k_preprocess
# IRAF script.

# USAGE detrend.csh filename
#
# filename is the name of the image to detrend.
#
# After de-trending the MEF file is split into subdirs.  The master 
# directory is set inside the param file of the IRAF task movesingle
# the root directory for a set of images is derived from the title of 
# the image


## Keep track of where this program was called from

set orig = `pwd`
set image = $1:r

# The second arg. is the type of work to do.
# R == Recovery, run through the pipe but don't plant
# N == No, just detrend, don't pipeline or plant objects
# Y == Yes, detrend should pass this image along to the pipeline
#      (Default is Y)

set dto = $2

if ( $dto == "" ) then
   set pipe="yes"
   set plant="yes"
else if ( $dto == "R" ) then
   set pipe="yes"
   set plant="no"
else if ( $dto == "N" ) then
   set pipe="no"
   set plant="no"
else if ( $dto == "Y" ) then
   set pipe="yes"
   set plant="yes"
else
   echo $0
   echo "Unrecognized detrend option: $dto"
   echo "Exiting"
   exit
endif

## Create a lock file so we know that this image 
## is currently being processed.
set lock = $image".lock"

set finished = $image".detrended"
set proclist = "proc-these-files"
set detectinput = "detect_input.txt"

if ( !(-e $finished) && !(-e $lock) ) then

cd ~/iraf

set term = none

cl << EOF

flpr

cd $orig

kinky 
pipeline

    touch $lock
    movesingle.pipeline=$pipe
    movesingle.plant=$plant

    cfh12k  $image

    touch $finished
    delete $lock verify-


logout

EOF

cd $orig

if ( !(-d Processed) ) then
   mkdir Processed
endif

if ( -e $image.detrended ) then
   mv $image.fits Processed/
endif 

endif

