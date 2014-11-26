#!/bin/bash

initdir=`pwd`
newdir="."

while [ $# -gt 0 ]; do
  case "$1" in
  -d)
    shift
    newdir=$1
    shift
    ;;
  -rn)
    shift
    rmin=$1
    shift
    ;;
  -rx)
    shift
    rmax=$1
    shift
    ;;
  -ang)
    shift
    ang=$1
    shift
    ;;
  -aw)
    shift
    aw=$1
    shift
    ;;
  -h|-?)
    echo "Usage: findall.sh [-d directory] [-rn min_rate] [-rx max_rate] [-ang angle] [-aw openning] [-h|-?]"
    exit
    ;;
  esac
done

cd $newdir
basedir=`pwd`

if [ X"$rmax" == X ]; then
  rmax="6.0"
fi
if [ X"$rmin" == X ]; then
  rmin="1.0"
fi
if [ X"$ang" == X ]; then
  ang="0.0"
fi
if [ X"$aw" == X ]; then
  aw="40.0"
fi

tailbin=`which tail`
ID=`date +"%H%M%S"`
ln -s ${tailbin} findall.tail.${ID}
\rm -f $basedir/processed
touch $basedir/processed

findall.tail.${ID} -n +1 -f $basedir/find.input | (
  while read a pr b wd; do
    if [ X"$a" = XDONE ]; then
      pid=`/sbin/pidof -o $$ -o $PPID -o %PPID -x findall.tail.${ID}`
      if [ "$pid" != "" ] ; then
        \rm -f findall.tail.${ID}
	kill $pid
	exit
      fi
    fi
    pr=`echo $pr | sed "s/'//g"`
    if [ X"$pr" = X ]; then
      echo "processfiles.sh -d $wd -rn $rmin -rx $rmax -ang $ang -aw $aw"
      processfiles.sh -d $wd -rn $rmin -rx $rmax -ang $ang -aw $aw
    else
      echo "processfiles.sh -p $pr -d $wd -rn $rmin -rx $rmax -ang $ang -aw $aw"
      processfiles.sh -p $pr -d $wd -rn $rmin -rx $rmax -ang $ang -aw $aw
    fi
    proc_ret=$?
    if [ $proc_ret = 0 ]; then
      echo $wd >> $basedir/processed
    fi
  done
)

echo "DONE" >> $basedir/processed

cd $initdir

exit
