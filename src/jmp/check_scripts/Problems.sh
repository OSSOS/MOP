#! /bin/bash

\rm -f Problems.trans
for d in chip??/*; do
  if [ -d $d ]; then
    cd $d
    for t in ??????[op]_??.trans.jmp; do
      if [ -e $t ]; then
        s=`awk '{ if (($1 > 200) || ($1 < -200) || ($4 > 200) || ($4 < -200)) {print 1} else {print 0} }' $t`
        if [ "X${s}" == "X1" ]; then
          echo "Problem with $d/$t" >> ../../Problems.trans
	  cat $t >> ../../Problems.trans
        fi
      fi
    done
    cd ../../
  fi
done

\rm -f ProblemsFK.trans
for d in chip??/*; do
  if [ -d $d ]; then
    cd $d
    for t in fk??????[op]_??.trans.jmp; do
      if [ -e $t ]; then
        s=`awk '{ if (($1 > 200) || ($1 < -200) || ($4 > 200) || ($4 < -200)) {print 1} else {print 0} }' $t`
        if [ "X${s}" == "X1" ]; then
          echo "Problem with $d/$t" >> ../../ProblemsFK.trans
	  cat $t >> ../../ProblemsFK.trans
        fi
      fi
    done
    cd ../../
  fi
done

\rm -f Problems.found
for d in chip??/*; do
  if [ -d $d ]; then
    cd $d
    for t in *comb.found; do
      if [ -e $t ]; then
        s=`cat $t | wc -l`
        if [ $s -lt 10 ]; then
          echo "$d/$t $s" >> ../../Problems.found
        fi
      fi
    done
    cd ../../
  fi
done

grep -v fka Problems.found | awk '{if ($2 < 4) {print $1}}' | sed "s|/fk| |" | awk '{print $1}' > Problems.area

exit
