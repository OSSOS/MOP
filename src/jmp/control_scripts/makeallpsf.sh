#!/bin/bash

initdir=`pwd`
pref=""
suf=""
newdir="."
plant="NO"
force="no"

while [ $# -gt 0 ]; do
  case "$1" in
  -d)
    shift
    newdir=$1
    shift
    ;;
  -p)
    shift
    pref=$1
    shift
    ;;
  -s)
    shift
    suf=$1
    shift
    ;;
  -f)
    shift
    plant=$1
    shift
    ;;
  --force)
    shift
    force="yes"
    ;;
  esac
done

cd $newdir
basedir=`pwd`
tailbin=`which tail`
ID=`date +"%H%M%S"`
ln -s ${tailbin} makeallpsf.tail.${ID}

makeallpsf.tail.${ID} -n +1 -f $basedir/directory | (
  while read wd; do
    if [ X"$wd" = XDONE ]; then
      pid=`/sbin/pidof -o $$ -o $PPID -o %PPID -x makeallpsf.tail.${ID}`
      if [ "$pid" != "" ] ; then
        \rm -f makeallpsf.tail.${ID}
	kill $pid
	exit
      fi
    fi
    echo $wd
    cd $wd
    for im in $pref*$suf.fits; do
      cd $basedir
      if [ "$force" = yes ]; then
	jmpmakepsf.csh $wd $im $plant force
      else
	jmpmakepsf.csh $wd $im $plant
      fi
      cd $wd
    done
    cd $basedir
  done
)

echo "DONE" >> $basedir/plant.input

cd $initdir

exit
