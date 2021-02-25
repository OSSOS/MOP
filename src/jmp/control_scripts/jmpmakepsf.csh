#!/bin/csh
echo $0 $*
# Builds a psf of image $2 which is waiting in directory $1
# 
# This processes depends on step0matt exiting.

touch jmpmakepsf.csh.FAILED

# This section added to enable finding the irafcl command
# required since the configuration of IRAF environment has
# now moved outside conda
# Unset variable that are used by IRAF
unset host iraf hbin hlib
unset mach arch IRAFARCH IRAFBIN

set irafbin = /usr/lib/iraf/bin/

if ( -x ${irafbin}vocl.e ) then
   set cl_binary = "${irafbin}vocl.e"
else if ( -x ${irafbin}ecl.e ) then
   set cl_binary = "${irafbin}ecl.e"
else
    set cl_binary = "${irafbin}cl.e"
endif



set iraf = "/usr/lib/iraf/"
set host = "${iraf}unix/"
set hlib = "${host}hlib/"
set hbin = "${host}bin"
set F77 = "$hlib/f77.sh"
set F2C = "$hbin/f2c.e"

set iraf_version = `grep version\\s  ${iraf}unix/hlib/zzsetenv.def | cut -d\" -f2 | cut -d" "  -f3`

#### END OF IRAF ENVIRONMENT SETUP.

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
set maxlin="20000."

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

# Is this area locked.. don't start
if ( -e $dir/$lock ) then 
   exit
endif

# Does PSF exist and we are NOT in force mode?
if ( -e $dir/$psf && $force == "no" ) then 
   touch jmpmakepsf.csh.OK
   exit
endif

## Now using Gwyn's ZP, if available
echo "Getting SGWYN ZP from ${image}"
set zerop=`gethead $dir/${image}.fits PHOTZP`
if ( "X$zerop" == "X" ) then
    echo -n "Didn't find SGWYN ZP, Trying to set zeropoint by scaling ELIXIR value, got: "
    set zerop=`gethead $dir/${image}.fits PHOT_C`
    if ( "X$zerop" == "X" ) then
        set zerop=`gethead $dir/${image}.fits PHOT_C0`
    endif
    if ( "X$zerop" == "X" ) then
        set zerop="26.0"
    endif
    set exptime=`gethead $dir/${image}.fits EXPTIME`
    set airmass=`gethead $dir/${image}.fits AIRMASS`
    set phot_k=`gethead $dir/${image}.fits PHOT_K`
    if ( "X$exptime" == "X" || "X$airmass" == "X" || "X$phot_k" == "X" ) then 
        set zerop="30.0"
    else
        set zerop=`echo $zerop $exptime $phot_k $airmass | awk ' { print $1 + 2.5*log($2)/log(10) + $3*($4-1) } ' `
    endif
    if ( "X$zerop" == "X" ) then
        set zerop="30.0"
    endif
    echo $zerop
endif

set term = none 


cd ~/iraf

$cl_binary << EOF

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
  jmpmakepsf.swidth=1
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
