#! /bin/bash

execute_prog() {
  $@
  if [ -f $1.OK ]; then
    \rm $1.OK $1.FAILED
    return 0
  else
    prog=$1
    shift
    echo "$prog failed with $@" > FAILED
    return 1
  fi
}

initdir=`pwd`
basedir=`pwd`
tailbin=`which tail`
ID=`date +"%H%M%S"`
ln -s ${tailbin} runcheck.tail.${ID}

#pwd

\rm -f BAD_TRANS-list FAILED-list
touch BAD_TRANS-list
touch FAILED-list
runcheck.tail.${ID} -n +1 -f ${basedir}/dir-list | (
  while read wd; do
    cd ${basedir}
#    pwd
    if [ X"$wd" = XDONE ]; then
      pid=`/sbin/pidof -o $$ -o $PPID -o %PPID -x runcheck.tail.${ID}`
      if [ "$pid" != "" ] ; then
        \rm -f runcheck.tail.${ID}
	kill $pid
	exit
      fi
    fi
    cd $wd
#    pwd
    \rm -f BAD_TRANS
    execute_prog checktrans
    if [ -e BAD_TRANS ]; then
      echo "$wd/BAD_TRANS" >> ${basedir}/BAD_TRANS-list
    fi
    if [ -e FAILED ]; then
      echo "$wd/FAILED" >> ${basedir}/FAILED-list
    fi
    \rm -f BAD_TRANSfk
    execute_prog checktrans -p fk
    if [ -e BAD_TRANSfk ]; then
      echo "$wd/BAD_TRANSfk" >> ${basedir}/BAD_TRANS-list
    fi
    if [ -e FAILED ]; then
      echo "$wd/FAILED" >> ${basedir}/FAILED-list
    fi
    cd $basedir
  done
)

cd ${initdir}

exit
