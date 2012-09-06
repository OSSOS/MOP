#!/bin/sh

initdir=`pwd`
irafdir=$HOME/iraf
iterm=$TERM
TERM="unknown"

if [ $# -gt 0 ]; then
  cd $1
fi

basedir=`pwd`
\rm -f directory
touch directory

for d in *; do
  if [ -d $d ]; then
    cd $d
    for c in chip??; do
      if [ -d $c ]; then
        cd $c
        wd=`pwd`
        echo $wd >> $basedir/directory
        cd ..
      fi
    done
    cd ..
  fi
done

echo "DONE" >> $basedir/directory

cd $initdir
TERM=$iterm

exit
