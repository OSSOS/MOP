#! /bin/sh

pref=""
if [ $# -ge 1 ]; then
  pref=$1
  shift
fi

for d in chip??/*; do
    if [ -d $d ]; then
    cd $d
    for i in ${pref}*.fits; do
	imcopy $i[1] bid
	\mv bid $i
    done
    for i in ${pref}*.fits.fz; do
        j=`echo $i | sed "s/.fz//"`
        if [ ! -f $j ]; then
	    imcopy $i[1] $j
        fi
        if [ -f $j ]; then
            size=`wc -c $j | cut -d " " -f 1`
	    if [ ${size} -gt 18300000 ]; then
	        \rm -f $i
#      \mv $j D/
	    else
	        \rm -f $j
	    fi
        fi
    done
    cd ../../
    fi
done

exit
