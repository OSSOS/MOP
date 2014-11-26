#!/bin/bash

initdir=`pwd`
newdir="."
mmag="20.0"
smag="0.5"

while [ $# -gt 0 ]; do
  case "$1" in
  -d)
    shift
    newdir=$1
    shift
    ;;
  -m)
    shift
    mmag=$1
    shift
    ;;
  -s)
    shift
    smag=$1
    shift
    ;;
  -h|-?)
    echo "Usage: jmpalleff.sh [-d directory] [-m min_mag] [-s step_mag] [-h|-?]"
    exit
    ;;
  esac
done

cd $newdir
basedir=`pwd`
tailbin=`which tail`
ID=`date +"%H%M%S"`
ln -s ${tailbin} jmpalleff.tail.${ID}
\rm eff.list
touch eff.list

jmpalleff.tail.${ID} -n +1 -f $basedir/processed | (
  while read wd; do
    if [ X"$wd" = XDONE ]; then
      pid=`/sbin/pidof -o $$ -o $PPID -o %PPID -x jmpalleff.tail.${ID}`
      if [ "$pid" != "" ] ; then
        \rm -f jmpalleff.tail.${ID}
	kill $pid
	exit
      fi
    fi
    echo $wd
    cd $wd
    f1=`ls *.comb.found | awk 'NR==1 { print $0 }' | sed 's/.comb.found//'`
    if [ ! -e $f1.comb.found ]; then
      echo "Directory not yet processed. Skipping."
    else
      pipe-eff -f $f1 -m $mmag -s $smag
      cd $basedir
      echo "$wd/$f1" >> eff.list
    fi
    cd $basedir
  done
)

echo "DONE" >> eff.list

cd $initdir

exit
