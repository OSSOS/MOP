#! /bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: create_surveys.sh <block>"
  echo "       where <block> is the short name of the block, i.e. there exist"
  echo "       a file <block>*.mag-rate.eff and it will create"
  echo "       <block>.sur."
  echo "       The chip?? directories are supposed to be in the current"
  echo "       directory."
  exit
fi

sur=$1
if [ -e ${sur}.sur ]; then
  \rm -f ${sur}.sur
fi
touch ${sur}.sur

curdir=`pwd`
for i in ${sur}*.mag-rate.eff; do
  if [ ! -e $i ]; then
    echo "Required file $i doesn't exists."
    exit
  fi
done
cd chip00
for f in `find . -name "fk*_??.comb.found" -print`; do
  h=`echo $f | sed "s/.comb.found/.mopheader/"`
  awk -v fi=${i} -f ~/Petit/extract-ra-dec-time.awk $h >> ${curdir}/${sur}.sur
done
cd ${curdir}

exit
