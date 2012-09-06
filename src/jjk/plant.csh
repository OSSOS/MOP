#!/bin/csh
# JJK july 2001
# This is very "brute force".  Teeter teeter.. we all fall down.
# move into the directory where the images are and run the pipeplant 
# program there.  The user is responsible to set the angle and rate

# parameters in the planting script.

set lock = "plant_lock"
set finished = "plant_finished"

# Calling sequence:  plant_operation.csh directory
# plant.csh workdir -rmin rate_min -rmax rate_max -angle angle -angle_width angle_width 
# or... just plant.csh workdir and then let the 'rate.pl' script figure stuff out.
# where directory is the location where a file called
# proc-these-files exists and contains a list of images to plant
# fake moving objects into.

set wdir = $1
set dir = `pwd`
if ( $#argv == 9 && $3 > 0 ) then 
 set rmin = $3
 set rmax = $5
 set ang = $7
 set aw = $9
else
## base the rates on 25 AU (rmax and awidth) and 200AU (rmin and Angle)
 set au_min=30
 set au_max=250
 set file = `grep -v "#" $wdir/proc-these-files | head -1 | awk ' { print $1 }'`
 set file = $file.fits
 set rmin = `rate.pl --file $wdir/$file $au_max | awk ' { printf "%8.1f",$4 }' `
 set rmax = `rate.pl --file $wdir/$file $au_min | awk ' { printf "%8.1f", $5 } ' `
 set ang = `rate.pl --file $wdir/$file $au_max | awk ' { printf "%8.1f", $2 } ' `
 set aw = `rate.pl --file $wdir/$file $au_min | awk ' { printf "%8.1f", $3 } ' `
 echo "Using rmin=$rmin rmax=$rmax ang=$ang width=$aw "
endif 

# match the .phot files left behind by the makepsf script.


# Should we plant or pass along to find? 

set nl = `grep -v "#" $wdir/proc-these-files | grep "NO" | wc -l | awk ' { print $1 } '`

if ( ! -e find.input ) then
    touch find.input
endif

if ( $nl == "0" ) then 

cd $wdir
# a lock or finished file indicates that this script has
# alread be started or finished in this directory.  
if ( -e $lock || -e $finished )  exit;

touch $lock

match.pl  -f proc-these-files

# create a list of random objects
if ( -e Object.planted ) \rm Object.planted

set im = `awk 'NR == 3 {print $1}' proc-these-files`
#set nx = `gethead NAXIS1 $im.mopheader`
set nx = 2080
#set ny = `gethead NAXIS2 $im.mopheader`
set ny = 4612
set pixs = `gethead PIXSCALE $im.mopheader`
# here is a typical KBO planting line
set rand = `date '+%N'`
kbo_gen 33  $nx 1 $ny  $rmin $rmax $ang $aw 21.0 23.5  5  5 $rand $pixs  > Object.planted
set rand = `date '+%N'`
kbo_gen 33  $nx 1 $ny  $rmin $rmax $ang $aw 23.5 25.5 25 25 $rand $pixs | grep -v "#" >> Object.planted
# Old kbo_gen version:
#
#    x_min x_max y_min y_max rate_min rate_max ang_min ang_max mag_min mag_max n_min n_max sd pixscale
#
# But this is not consistant with other programs, and with normal meaning of
# $ang and $aw. So changed kbo_gen to take mean angle and half width:
#
#    x_min x_max y_min y_max rate_min rate_max ang_mean ang_width mag_min mag_max n_min n_max sd pixscale

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

logout

EOF

cd $dir

cd $wdir

\rm $lock

touch $finished
touch plant.OK

cd $dir

echo "-p fk -d $wdir " >> find.input

else

echo "No planting... just find"

echo "-p '' -d $wdir " >> find.input

endif

