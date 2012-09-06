#! /bin/csh

# Builds a psf of image $2 which is waiting in directory $1
# 
# This processes depends on step0matt exiting.

set orig = `pwd`
set dir = $1
set image = $2:r
set plant = $3
set force = "no"
if ( $#argv == 4 ) then
    if ( $4 == "force" ) set force = "yes"
endif

set fwhm="4."
set thresh="4."
set maxlin="40000."

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

if ( -e $dir/$lock || -e $dir/$psf && $force == "no" ) then 
   exit
endif

set zerop=`gethead $dir/$2 PHOT_C`
if ( "X$zerop" == "X" ) then
    set zerop="26.0"
endif

set term = none 

echo "jmpmakepsf.csh $dir $image $plant"

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
  cache ("jmpprepimage")

  jmpprepimage.plant=$plant
  jmpmakepsf.zeropt=$zerop
  jmpprepimage $image $fwhm $thresh $maxlin
  delete ./piejmpmaf.par verify-
  delete $lock verify-

  wc -l proc-these-files  | scan(s1)

  if ( s1=="5" ) {
    touch $output
    print "$dir" >> $output
  }
  ;

logout

EOF

