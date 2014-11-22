#!/bin/bash

initdir=`pwd`
pref=""
suf=""
newdir="."
plant="NO"
seeing_o="1.0"

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
    seeing_o=$1
    shift
    ;;
  -h|-?)
    echo "Usage: initfind.sh [-d directory] [-p prefix] [-s suffix] [-f seeing] [-h|-?]"
    exit
    ;;
  esac
done

execute_prog() {
  $@
  if [ -f $1.OK ]; then
    \rm $1.OK $1.FAILED
    return 0
  else
    prog=$1
    shift
    echo "$prog failed with $@" > FAILED
    exit 1
  fi
}

cd $newdir
basedir=`pwd`
tailbin=`which tail`
ID=`date +"%H%M%S"`
ln -s ${tailbin} initfind.tail.${ID}
\rm $basedir/find.input
touch $basedir/find.input

initfind.tail.${ID} -n +1 -f $basedir/directory | (
  while read wd; do
    if [ X"$wd" = XDONE ]; then
      pid=`/sbin/pidof -o $$ -o $PPID -o %PPID -x initfind.tail.${ID}`
      if [ "$pid" != "" ] ; then
        \rm -f initfind.tail.${ID}
	kill $pid
	exit
      fi
    fi
    echo $wd
    cd $wd
    echo "# Files to be planted and searched" > proc-these-files
    echo "#             image  fwhm plant" >> proc-these-files
    for im in $pref*$suf.fits; do
      if [ ! -e ${im/.fits/}.mopheader ]; then
        execute_prog stepZjmp -f ${im/.fits/}
      fi
      execute_prog step0jmp -f ${im/.fits/} -w 4.5 -t 4. -m 20000.
      if [ -e ${im/.fits/}.fwhm ]; then
        read seeing_c < ${im/.fits/}.fwhm
	seeing=$seeing_c
      else
        seeing=$seeing_o
      fi
      echo " ${im/.fits/} ${seeing} NO" >> proc-these-files
    done
    cd $basedir
    echo "-p '' -d $wd " >> $basedir/find.input
  done
)

echo "DONE" >> $basedir/find.input

cd $initdir

exit
