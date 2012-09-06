#!/bin/csh

# Builds a psf of image $2 which is waiting in directory $1
# 
# This processes depends on step0matt exiting.

set orig = `pwd`
set dir = $1
set image = $2:r
set plant = $3

if ( $?plant ) then
 if ( $plant == "YES" ) then 
    set plant = "yes"
 else 
    set plant = "no"
 endif
else
  set plant = "yes"
endif

set lock = $image"_lock"
set psf = $image".psf.fits"
set output = $orig"/plant.input"

if ( -e $dir/$lock || -e $dir/$psf ) then 
   exit
endif


set term = none 

echo $dir $image

cd ~/iraf

cl << EOF

flpr

kinky  
pipeline

cd $orig
cd $dir

# If lock exists then makepsf is already working in this directory.
# if a psf files exists then we're already done here.

  touch $lock
  print $plant
  cache ("prepimage")

  prepimage.plant=$plant
  prepimage $image
  delete $lock verify-

  wc -l proc-these-files  | scan(s1)

  if ( s1=="5" ) {
    touch $output
    print "$dir" >> $output
  }
  ;

logout

EOF

