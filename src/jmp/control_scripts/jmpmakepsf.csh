#! /bin/csh
echo $0 $*
# Builds a psf of image $2 which is waiting in directory $1
# 
# This processes depends on step0matt exiting.

touch jmpmakepsf.csh.FAILED

set orig = `pwd`
set dir = $1
set image = $2:r
set plant = $3
set force = "no"
if ( $#argv == 4 ) then
    if ( $4 == "force" ) set force = "yes"
    touch jmpmakepsf.csh.OK
endif

set fwhm="4."
set thresh="4."
set maxlin="59000."

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

## Now using Gwyn's ZP, if available
echo "Getting SGWYN ZP from ${image}"
set zerop=`gethead $dir/${image}.fits PHOTZP`
if ( "X$zerop" == "X" ) then
    echo -n "Didn't find SGWYN ZP, Trying to set zeropoint by scaling ELIXIR value, got: "
    set zerop=`gethead $dir/${image}.fits EXPTIME PHOT_C0 | awk ' { print 2.5*log($1)/log(10)+$2 } ' `
    echo $zerop
    if ( "X$zerop" == "X" ) then
        set zerop="26.0"
    endif
endif

set term = none 


cd ~/iraf

ecl << EOF

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
  makepsf.zeropt=$zerop
  jmpprepimage $image $fwhm $thresh $maxlin
  delete ./piejmpmaf.par verify-
  delete $lock verify-

  wc -l proc-these-files  | scan(s1)

  if ( s1=="5" ) {
    touch $output
    print "$dir" >> $output
  }
  ;

  touch jmpmakepsf.csh.OK
logout

EOF
