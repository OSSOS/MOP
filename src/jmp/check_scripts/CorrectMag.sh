#! /bin/bash

CorrectMag() {
  cat1=$1
  cat2=$2

  zp1=`cat zeropoint.used`
  zp2=`cat zeropoint.corrected`

  awk "{ printf \"%10.2f%10.2f%10.2f%10.2f%10.2f\n\", \$1, \$2, \$3+$zp2-$zp1, \$4, \$5 }" ${cat1} > ${cat2}
}

initdir=`pwd`
basedir=`pwd`
tailbin=`which tail`
ID=`date +"%H%M%S"`
ln -s ${tailbin} correct-mag.tail.${ID}

#pwd

correct-mag.tail.${ID} -n +1 -f ${basedir}/dir-list | (
  while read wd; do
    cd ${basedir}
#    pwd
    if [ X"$wd" = XDONE ]; then
      pid=`/sbin/pidof -o $$ -o $PPID -o %PPID -x correct-mag.tail.${ID}`
      if [ "$pid" != "" ] ; then
        \rm -f correct-mag.tail.${ID}
	kill $pid
	exit
      fi
    fi
    cd $wd
#    pwd
    for i in *_??.comb.missed; do
      if [ -e $i ]; then
        filename=`echo $i | sed "s/.comb.missed//"`
        echo "CorrectMag $filename"
        CorrectMag $filename.comb.found $filename.corrected.comb.found
        CorrectMag $filename.comb.missed $filename.corrected.comb.missed
	if [ ! -e $filename.corrected.mopheader ]; then
	    ln -s $filename.mopheader $filename.corrected.mopheader
	fi
      fi
    done
    cd $basedir
  done
)

cd ${initdir}

exit
