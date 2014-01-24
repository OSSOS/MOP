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
if ( $#argv == 5) then 
 set rmin = $2
 set rmax = $3
 set ang = $4
 set aw = $5
else
 echo "bad calling sequence"
 echo "plant.csh wdir rmin rmax ang awidth"
 exit -1
endif

# match the .phot files left behind by the makepsf script.


# Should we plant or pass along to find? 

set nl = `grep -v "#" $wdir/proc-these-files | grep "NO" | wc -l | awk ' { print $1 } '`
echo "Read $nl lines from proc-these-files"


if ( $nl == "0" ) then 

cd $wdir
# a lock or finished file indicates that this script has
# alread be started or finished in this directory.  
if ( -e $lock || -e $finished )  exit;

touch $lock

match.pl

# create a list of random objects
if ( -e Object.planted ) \rm Object.planted

set im = `grep -v "#" proc-these-files | awk ' NR == 1 { print $1 } ' `
# note DATASEC = '[33:2080,1:4612]'
set nx = 2080
set ny = 4612
set pixs = `gethead PIXSCALE $im.mopheader`
set dataset=(`gethead DATASEC $im.fits | sed -e 's/[\,\:\[\]]*/] /g' | awk -F] ' { print $2 $3 $4 $5 } ' `)
# here is a typical KBO planting line
echo "Hard coded for MegaPrime untrimmed images, May 2013"
set rand=`date '+%N'`
kbo_gen ${datasec[0]} ${datasec[1]} ${datasec[2]} ${datasec[3]} $rmin $rmax $ang $aw 21.0 23.5 10 10 $rand $pixs  > Object.planted
set rand=`date '+%N'`
kbo_gen ${datasec[0]} ${datasec[1]} ${datasec[2]} ${datasec[3]}  $rmin $rmax $ang $aw 23.5 25.2 25 25 $rand $pixs | grep -v "#" >> Object.planted
set ravg=`echo $rmax $rmin | awk ' { print( ($1 - $2)/2.0 ) } '`
echo $ravg
set rand=`date '+%N'`
kbo_gen ${datasec[0]} ${datasec[1]} ${datasec[2]} ${datasec[3]} $rmin $ravg $ang $aw 23.5 25.2 25 25 $rand $pixs | grep -v "#" >> Object.planted


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

cd $wdir

\rm $lock


cd $dir

echo "-p fk -d $wdir " >> find.input

else

echo "No planting... just find"

echo "-p '' -d $wdir " >> find.input

endif

