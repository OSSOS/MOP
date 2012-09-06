#!/bin/sh

f1=$1
f2=$2
f3=$3

goodsols=0

exec_file_prog() {
  $@
  if [ -f $2.$1.OK ]; then
    \rm $2.$1.OK $2.$1.FAILED
    goodsols=$(($goodsols+1))
    echo "goodsols " $goodsols
    return 0
  else
    prog=$1
    shift
    echo "$prog failed with $@" > FAILED
    exit 1
  fi
}

if [ ! -f aper.corr ]; then
    touch aper.corr
    cat $f1.apcor >> aper.corr
    cat $f2.apcor >> aper.corr
    cat $f3.apcor >> aper.corr
fi

echo "Plate solution"

exec_file_prog mkpltsol $f1
exec_file_prog mkpltsol $f2
exec_file_prog mkpltsol $f3

if [ $goodsols -gt 2 ]; then
    exec_file_prog measure3 $f1
fi

exit
